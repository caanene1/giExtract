The giFeature script in this folder spefically identifies histological features that significantly associated with sample group. This is speficially for the results reported in the mansucript.

The script can be called from commandline and it takes ordered arguments as below 
Rscript giFeature context cnnfeature mergeColumn slideColumn groupColumn.

Please note the order of the inputs are important and they correspond to below information.
context - Path to csv file with meta information. This table must have only 3 columns Name, slide and Group.

cnnfeature - Path to csv file with cnn features to analyse. This table must be an output of giExtract. It should contain a column with matching name as the first column in context file.

mergeColumn - The column name of the matching column in context and cnnfeature files above to merge the table. It is ideal to change the context name to Name as the giExtract will use this for its output.

slideColumn - The column name of the slide name in meta file defined in the context file.

groupColumm - The column name of the Group name in meta file define in the context file.


Alternatively, you can also run the function directly inside R without using commandline.




