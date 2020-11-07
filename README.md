# RAMPDATA

Reads RAMP project data from an archive.

Install the 7zip tool by hand.

```
devtools::install_github("jimhester/archive")
```

Ramp image from https://www.flickr.com/photos/chrisfurniss/.

# Development

This package uses Renv, so it will try to install the packages it needs locally into
the renv folder.

This package is set up for development in a Docker container when using VisualStudio Code,
so VS Code will try to install and run a Docker container when you open it.
If you haven't used Renv before, you should create `mkdir ~/.local/share/renv` before opening
the project. Then the Docker container will try to mount this.
