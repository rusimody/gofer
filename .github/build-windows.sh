cd src
make clean
CC=/usr/bin/i686-w64-mingw32-gcc-win32 CFLAGS="-DWIN32=1 -DLINUX=0" make
mkdir -p pug_windows
cp gofer.exe ./pug_windows
cp ../windows/*.pre ./pug_windows
zip -r pug_windows.zip ./pug_windows
cd ..