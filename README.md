# Image viewer with slideshow (Windows/Linux/Mac)
Simple, minimum, yet configurable photo viewer/slideshow/digital signage software for Windows, Linux, and macOS. Inspired by "feh" https://feh.finalrewind.org/

## Download:
Windows version is [available at the Microsoft Store](https://apps.microsoft.com/store/detail/simple-image-viewer/9NNZPQD4WJCK).　

## Feature:  
* Multiple viewing styles: 1. Normal window view, 2. "Windowless" inFrame view with slideshow, and 3. Fullscreen view with slideshow.   
* Full keyboard and mouse playback control. 
* Configurable options with command line parameters and popup menus. 
* Supports multiple moniters.
* Files and folder drop support.

## Screenshot:  
Windowless(inFrame) View (with slideshow) on Windows 11  
![Windowless View (with slideshow) on Windows 11](https://github.com/torum/Image-viewer/blob/master/files/bin/ImageViewerScreenshot3-n.png?raw=true)

Normal View on Windows 11
![Normal View on Windows 11](https://github.com/torum/Image-viewer/blob/master/files/bin/ImageViewerScreenshot3-Windows.png?raw=true)

Normal View on Linux
![Normal View on Linux](https://github.com/torum/Image-viewer/blob/master/files/bin/ImageViewerScreenshot3-Ubuntu.png?raw=true)

## Usage:  
ImageViewer can be launched by (1) double clicking the executable and selecting image files or (2) selecting image files or folders and using "send to" feature in Windows explorer (create "shortcut" file and place it to "shell:sendto" folder) or (3) command-line.

### Command-line options:  
ImageViewer can be launched via command-line, and applications or scripts can also launch Image Viewer with following options.  
   
- Start fullscreen mode at startup (default off):  
`-f on`  or  `--fullscreen=on`   
`-f off`  or  `--fullscreen=off`   

- Start inFrame mode at startup (default off):  
`-s on`  or  `--inFrame=on`   
`-s off`  or  `--inFrame=off`  

- Start/Force slideshow (default behavior: if single file is selected = off, if folder is selected = on):  
`-a on`  or  `--slideshowAutoStart=on`   
`-a off`  or  `--slideshowAutoStart=off`  

- Set slideshow interval in seconds (default 4 seconds):  
`-i 4`  or `--interval=4`   
  
- Set slideshow random (default on):  
`-r on`  or  `--random=on`   
`-r off`  or  `--random=off`   
  
- Set slideshow repeat (default on):  
`-e on`  or  `--repeat=on`   
`-e off`  or  `--repeat=off`   

- Set slideshow transitional effect (default on):  
`-t on`  or  `--effect=on`   
`-t off`  or  `--effect=off`   
  
- Picture stretch In (fit to window/screen when the size is bigger than window/screen. default on):  
`-n on`  or  `--stretchIn=on`   
`-n off`  or  `--stretchIn=off`   
  
- Picture stretch Out (fit to window/screen when the size is smaller than window/screen. default off):  
`-o on`  or  `--stretchOut=on`   
`-o off`  or  `--stretchOut=off`   
  
- Load pictures in the sub folders as well when manually open a picture (default on):  
`-u on`  or  `--includSubFolders=on`   
`-u off`  or  `--includSubFolders=off`   
  
- Specify a moniter to show fullscreen slideshow (default 0 is the main moniter):  
`-m 1`  or  `--moniter=1`    
  
- Specify window should stay on top (default off):  
`-y on`  or  `--stayOnTop=on`   
`-y off`  or  `--stayOnTop=off`   

- Help shows command-line options.  
`-h`  or  `--help`   
  
### Command-line Useage example:    
` $ ImageViewer -i 2 -f on -o on -e off C:\Users\<USER>\Pictures\Wallpapers\` 

##  Contributing 
Feel free to open issues and send PRs. It is much appreciated. 

### Build instruction:   
You can download the Lazarus IDE and FPC(compiler) from [here](https://www.lazarus-ide.org/index.php?page=downloads).  

Or `sudo apt install lazarus` on Ubuntu.

Just open the project file ("ImageViewer.lpr") with Lazarus IDE and hit F9 and run. That's it!

If you are on Linux, you may need to change target platform. Main menu > Project > Project Options and Compiler Options > Config and Target menu, and then pick Target platform > Target OS "Linux", Target CPU family "x86_64" according to your system.
 
### Compiled and tested on:   

* Windows 11: Lazarus 2.2.2 FPC 3.2.2 x86_64-win64-win32/win64
* Windows 10: Lazarus 1.8.0 FPC 3.0.4 x86_64-win64-win32/win64
* Ubuntu 22.04.1 LTS: Lazarus 2.2.0 FPC 3.2.2 x86_64-linux-gtk2
* Ubuntu 17.10 (64bit): Lazarus 1.8.0 FPC 3.0.2 x86_64-linux-gtk2
* Ubuntu 16.04 LTS (64bit): Lazarus 1.9.0 trunk, FPC 3.0.4
* macOS 10.13.3 (64bit) High Sierra: Lazarus 1.8.0 rexported FPC 3.0.4 i386-darwin-carbon
* macOS 10.11.6 (64bit) El Capitan: Lazarus 1.9.0 carbon trunk, FPC 3.0.4

## Contributers
- [@Nemo08](https://github.com/Nemo08) Russian translation.  
- [@quadroid](https://github.com/quadroid) File/folder drop and portable mode.  

## Third-party components
[win32titlestyler.pas](https://github.com/Alexey-T/CudaText/blob/master/comp/win32titlestyler.pas) by [@Alexey-T](https://github.com/Alexey-T) 

