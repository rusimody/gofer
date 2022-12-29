cd src
make clean
make
mkdir -p pug_linux
cp gofer ./pug_linux
cp ../*.pre ./pug_linux
zip -r pug_linux.zip ./pug_linux
cd ..