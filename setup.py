import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

with open("requirements.txt") as rq:
    install_requires = rq.read()


setuptools.setup(
    name="giExtract",
    version="1.0.0",
    scripts=["bin/giExtract", "bin/giCube"],
    author="Chinedu A. Anene",
    author_email="caanenedr@outlook.com",
    description="Digital pathology feature extraction",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/caanene1/giExtract",
    download_url = "https://github.com/caanene1/giExtract/releases/download/1.0.6/giExtract-1.0.0-py3-none-any.whl",
    packages=setuptools.find_packages(include=["giExtract"]),
    include_package_data=True,
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent"
    ],
    install_requires=install_requires,
    python_requires='>=3.8',
)

# Build >> python3 setup.py sdist bdist_wheel
# Upload >> sudo twine upload dist/*

