# pathfinder_testing

Collection of stan-dev repo branches need to run pathfinder

Use the following commands to download and install this repo

```bash 
git clone --recursive git@github.com:SteveBronder/pathfinder_testing.git
# if you don't have ssh enabled on your local git use the following 
# git clone --recursive https://github.com/SteveBronder/pathfinder_testing.git 
cd cmdstan 
echo "STAN_THREADS=true" > make/local 
echo "O=3 -march=native -mtune=native" >> make/local 
make -j4 build
cd .. 
R -s -e "remotes::install_local(path = './cmdstanr')"
```

Hopefully, that should be it and you can then run the examples in this folder!
