# Steps to run on HPC for Chapter 2

1. Prepare files by running 01-prep/main.r with parameter necessary.

2. Copy folder `2020-08-31-jsa-type-v2-ch2/` using winSCP

3. Use following code
```
    ssh e0045029@atlas6-c01.nus.edu.sg
    cd ~
    cd /hpctmp2/e0045029
    qsub -q openmp -o 2020-08-31-jsa-type-v2-ch2/output-20201116.txt 2020-08-31-jsa-type-v2-ch2/main.sh
```

