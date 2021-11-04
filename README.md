<!--
*** We're using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** TODO: See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. Alternative option for links:
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stars][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

# itrackvalr: Toolkit to analyze value by tracking eye gaze (in R)

The `itrackvalr` package provides a collection of functions that make it easier to work with the various data sources used in value-based eye-tracking experiments.

**TODO items**

Plots for each tier of thresholding, such as 1.5 degrees and 2.5 degrees. Write
summaries of how validation-revalidation participant groups look. Start a separate RMarkdown to generate per-idea summaries.

- [ ] What groups of participants are we using based on thresholds?
- [ ] How do we do dimensional reduction?
- [ ] Apply offsets to gx/gy THEN flip to consistent side for all participants

## Prerequisites

You must have R and RStudio installed on your computer.

## Setup for project team

Clone this repository into your local project folder using GitHub Desktop or via the command line using

```sh
git clone https://github.com/sokolhessnerlab/itrackvalr.git
```

Then, open the `itrackvalr` directory and double-click on `itrackvalr.Rproj` to launch the project in RStudio. When RStudio first loads the project, it will source the included `.RProfile` to configure helpful defaults for your session.

### Loading package dependencies

We use the [`renv` package](https://rstudio.github.io/renv/index.html) to manage packages used for developing and using `itrackvalr`. To install the package dependencies for this project, run the following restoration command in the R console.

```r
renv::restore()
```

The dependencies are tracked by version in the `renv.lock` file. If during development you install a package that the project will depend on to run properly, please use `renv::snaptshot()` to update the lock file.

### Participant data

For the current studies, participant data is stored on a shared drive at the University of Denver. If you plan to work with participant data during your session, you must connect to the data separately from this package from whichever machine you are working from.

If working remotely, a secure connection to participant data requires connecting to the DU VPN and then mounting the shared drive to your computer.

## Code of Conduct

We are committed to fostering a welcoming community for the `itrackvalr` package.

[View our Code of Conduct](https://github.com/sokolhessnerlab/.github/tree/main/CODE_OF_CONDUCT.md) for our GitHub organization.

<!-- TODO: CONTRIBUTING -->
## Contributing

Please see our [contributing guide](./.github/CONTRIBUTING.md).

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.md` for more information.

<!-- CONTACT -->
## Contact

sokolhessnerlab@gmail.com

Project Link: [https://github.com/sokolhessnerlab/itrackvalr](https://github.com/sokolhessnerlab/itrackvalr)

<!-- ACKNOWLEDGMENTS -->
<!-- ## Acknowledgments -->

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/sokolhessnerlab/itrackvalr?style=for-the-badge
[contributors-url]: https://github.com/sokolhessnerlab/itrackvalr/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/sokolhessnerlab/itrackvalr?style=for-the-badge
[forks-url]: https://github.com/sokolhessnerlab/itrackvalr/network/members
[stars-shield]: https://img.shields.io/github/stars/sokolhessnerlab/itrackvalr?style=for-the-badge
[stars-url]: https://github.com/sokolhessnerlab/itrackvalr/stargazers
[issues-shield]: https://img.shields.io/github/issues/sokolhessnerlab/itrackvalr?style=for-the-badge
[issues-url]: https://github.com/sokolhessnerlab/itrackvalr/issues
[license-shield]: https://img.shields.io/github/license/sokolhessnerlab/itrackvalr?style=for-the-badge
[license-url]: https://github.com/sokolhessnerlab/itrackvalr/blob/main/LICENSE.md

