# free_college
The repositorty contains data and code associated with the paper "Shaping inequality and intergenerational persistence of poverty: Free college or better schools?" (by Dirk Krueger, Alexander Ludwig and Irina Popova) 

The folder “data” contains the raw data (or links to external repositories where it is stored) and codes needed to generate empirical results in the paper.
The folder contains:
(a) PSID data and related codes used to generate descriptive statistics cross-sectional moments for adult households as well as wage process estimates;
(b) CDS_PSID (I-III) data and related codes used to generate child-elated descriptive statistics used for model calibration;
(c) NSLY79 data used to estimate education-specific ability gradients of wages.
All data are publicly available.
2. The folder “model” contains the code used to generate model-based results in the paper. The folder contains the code used to solve and calibrate the
general equilibrium overlapping generations model, and to generate the results plots and tables.

Software Requirements
• Stata (code was last run with version 15.1)
– Run on Windows OS
– Package psidtools is required
• Intel Fortran Compiler (version used - Intel Fortran Compiler Classic 2019.5)
– Code run on Linux OS
– OpenMP Libraries required
– Code run with 40 hypothreads (respective shell script to set environment variables is provided)
– IMSL Fortran Numerical Library (RogueWave) is required (Version 2018.0 used)
• Matlab (code was run with Matlab Release 2018b)

Memory, CPU and Runtime Requirements
The Fortran code is not feasible to run on a desktop machine (ca. 500 GB RAM required; to make use of shared memory parallelization at least 10 physical cores are recommended). The code was run on a HPC infrastructrúre in Frankfurt and Bonn.
The Stata and Matlab codes can be run within few minutes on a desktop Windows machine.

Contact for Questions
For any questions regarding the codes and data please contact Irina Popova (ipopova@uni-bonn.de).

