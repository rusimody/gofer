cd src
make clean
make
rm -rf pug
mkdir -p pug
cp gofer ./pug
cp ../pusimple.pre ../pustd.pre ../pucc28.pre ./pug
zip -r pug_linux.zip ./pug
cd ..