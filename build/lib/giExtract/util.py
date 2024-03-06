"""
Name: giExtractor util
Author: Chinedu A. Anene, Phd
"""

from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.models import Model
from tensorflow.keras.applications import inception_resnet_v2, inception_v3
from tensorflow.keras.applications import VGG16, ResNet50
from tensorflow.keras.applications import densenet
import os.path
import cv2 as cv
from PIL import Image
import numpy as np
import pandas as pd
import os


def generator(x, s=(299, 299), b=5,
              path=None, file="path"):
    """
    Image generator for feeding a CNN built with Keras, flow dataframe
    :param x: Meta file with name and class columns
    :param s: Image size (width, height)
    :param b: Batch size
    :param file: Name of column with file names
    :param path: The path of the parent folder if, the col_path is not a full path.
    :return: Yields image and labels
    """

    gg = ImageDataGenerator(rescale=1.0/255,
                            samplewise_center=False,
                            samplewise_std_normalization=False,
                            featurewise_center=False,
                            featurewise_std_normalization=False,
                            data_format="channels_last")

    d_gg = gg.flow_from_dataframe(dataframe=x, directory=path, x_col=file,
                                  validate_filenames=True, y_col=None,
                                  target_size=s, class_mode=None, shuffle=False,
                                  save_format="JPEG", batch_size=b)
    return d_gg


def create_model(xn="feature"):
    """
    :param xn: Name to give the last pooling layer
    :return: The log-its outputs of the model (Default)
    """
    b_inception = inception_v3.InceptionV3(input_shape=(299, 299, 3), include_top=False,
                                           weights='imagenet', pooling="avg")
    b_inception.layers[-1]._name = xn

    b_inception_res = inception_resnet_v2.InceptionResNetV2(input_shape=(299, 299, 3),
                                                     include_top=False,
                                                     weights="imagenet", pooling="avg")
    b_inception_res.layers[-1]._name = xn

    b_dense = densenet.DenseNet121(input_shape=(224, 224, 3), include_top=False,
                                   weights='imagenet', pooling="avg")
    b_dense.layers[-1]._name = xn

    b_vgg = VGG16(input_shape=(224, 224, 3), include_top=False,
                  weights='imagenet', pooling="avg")
    b_vgg.layers[-1]._name = xn

    b_resnet = ResNet50(input_shape=(224, 224, 3), include_top=False,
                        weights='imagenet', pooling="avg")
    b_resnet.layers[-1]._name = xn


    ff = {"inception":Model(inputs=b_inception.input, outputs=b_inception.get_layer(xn).output),
          "inception_res":Model(inputs=b_inception_res.input, outputs=b_inception_res.get_layer(xn).output),
           "dense":Model(inputs=b_dense.input, outputs=b_dense.get_layer(xn).output),
          "vgg":Model(inputs=b_vgg.input, outputs=b_vgg.get_layer(xn).output),
          "resnet":Model(inputs=b_resnet.input, outputs=b_resnet.get_layer(xn).output)}

    return ff


def save_image(img, path):
    """
    Save images to file
    """
    if img is not None:
        if img.dtype == "bool":
            img = img.astype("uint8") * 255
        elif img.dtype == "float64":
            img = (img * 255).astype("uint8")

        img = Image.fromarray(img)
        img.save(path)
    return

class ImgWindow:
    """
     Class for sliding window on image
    """

    def __init__(self, i, d=92, n=5, m=20, c=20):
        """
        Sliding window class
        Parameters:
            i: image path
            d: dimension of slider
            n: Number of random cubes
            m: Tolerance for masking out white spaces
            c: Percentage of with space to remove
        """
        self.i = i
        self.d = d
        self.n = n
        self.m = m
        self.c = c
        self.img = cv.imread(self.i)
        self.__cubes = dict()

    def __content_percent(self, img):
        """
        Mask out pixels with similar red, green, and blue values.
        Args:
          :param img: RGB image as a NumPy array.
        Returns:
          Tuple of array for masked image and percentage mask.
        """

        if (len(img.shape) == 3) and (img.shape[2] == 3):
            rgb = img.astype(int)
            rg_diff = abs(rgb[:, :, 0] - rgb[:, :, 1]) <= self.m
            rb_diff = abs(rgb[:, :, 0] - rgb[:, :, 2]) <= self.m
            gb_diff = abs(rgb[:, :, 1] - rgb[:, :, 2]) <= self.m

            res = ~(rg_diff & rb_diff & gb_diff)
            res = 1 * res

            res = img * np.dstack([res, res, res])
            msk_per = round(100 - np.count_nonzero(res) / res.size * 100)
            # You can use "res" to return a masked image
        else:
            print("Input to this function must be 3d nd array")
            msk_per = -1

        return msk_per


    def __fixed_cube_get(self):
        """
        Get 2 * 2 fixed image cubes
        """
        n = 0
        dd = self.d
        for x in range(0, self.img.shape[0] - dd, dd):
            for y in range(0, self.img.shape[1] - dd, dd):
                window = self.img[x:x + dd, y:y + dd, :]

                if self.__content_percent(window) <= self.c:
                    self.__cubes["h" + str(n)] = window
                    n += 1

        return

    def __random_crop(self):
        """
        Generate random crop of images
        """

        im = self.img
        h, w = im.shape[0], im.shape[1]
        # put a guard to stop if the image is smaller than the cube dimension

        counter = self.n + 1

        while counter != 0:
            x = np.random.randint(0, w - self.d + 1)
            y = np.random.randint(0, h - self.d + 1)
            window = self.img[y:(y+self.d), x:(x+self.d), :]

            if self.__content_percent(window) <= self.c:
                self.__cubes["h" + str(counter)] = window
                counter -= 1
        return

    def fit(self, tp=("all", "random")):
        if tp == "all":
            self.__fixed_cube_get()
        elif tp == "random":
            self.__random_crop()
        else:
            print("Indicate the type, all or random?")
        return

    def get_cube(self):
        """
        Get the dictionary of cubes
        """
        return self.__cubes


def extract_cubes(path, s_f=".jpg", d=300, tp="random", n=20, m=20, c=20, folder=False):
    """
    Extract cubes from images
    Parameters:
      :param path: Image folder
      :param s_f: File format of the images
      :param d: Dimension of square cube
      :param tp: The type of extraction
      :param n: The number of random cubes
      :param m: The tolerance for masking white space
      :param c: The lowest allowed white space percentage
      :param folder: Store output in a folder or not
    """

    n_path = os.path.join(path, "cubes")

    files = [f for f in os.listdir(path) if f.endswith(s_f)]
    files = [f for f in files if not f.startswith(".")]

    print("Cubing {} images".format(len(files)))

    if not os.path.exists(n_path):
        os.makedirs(n_path)

    for key, i in enumerate(files):
        sv = i[:-3]
        sl = os.path.join(path, i)

        if folder:
            n_sl = os.path.join(n_path, i)

            if not os.path.exists(n_sl):
                os.makedirs(n_sl)

        else:
            n_sl = n_path

        cla = ImgWindow(sl, d=d, n=n, m=m, c=c)
        cla.fit(tp=tp)
        cubes = cla.get_cube()

        for z in cubes:
            save_image(cubes[z], os.path.join(n_sl, sv + z + s_f))
    return


def nat_com(pre):
    """
    Create the NatCom output for ESCA
    :param pre: The prediction table
    :return: Sum of the features
    """

    columns = ["var", "group"]

    data = np.array([["dense_133", "MET"], ["dense_472", "MET"],
                      ["dense_554", "MET"], ["inception_res_57", "MET"],
                      [ "dense_433", "MET"], ["dense_409", "DIFF"],
                     ["resnet_1863", "DIFF"], ["dense_429", "DIFF"],
                     ["resnet_1562", "DIFF"], ["vgg_352", "DIFF"],
                     ["dense_12", "IMM"], ["resnet_1509", "IMM"],
                     ["resnet_835", "IMM"], ["resnet_981", "IMM"],
                     ["inception_res_1096", "IMM"], ["resnet_904", "STEM"],
                     ["resnet_1544", "STEM"], ["resnet_1730", "STEM"],
                     ["resnet_1278", "STEM"], ["inception_res_120", "STEM"]])

    data = pd.DataFrame(data=data, columns=columns)

    output = pre[pre.columns.intersection(list(data["var"]))].copy()
    output["Name"] = pre["Name"]

    gx = pd.unique(data["group"])

    for i in gx:
        gf = data.loc[data["group"] == i]
        df = pre[pre.columns.intersection(gf["var"])]
        output[i] = df.sum(axis=1)

    return output















