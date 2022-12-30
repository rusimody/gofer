cd src
make clean
CC=/usr/bin/i686-w64-mingw32-gcc-win32 CFLAGS="-DWIN32=1 -DLINUX=0" make
rm -rf pug
mkdir -p pug
cp gofer.exe ./pug
cp ../windows/*.pre ./pug
cd pug
# Zip all files in the pug directory
zip -r ../pug_windows.zip *
cd ../..