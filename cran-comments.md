## Resubmission

This is a resubmission. In this version:

* I added three references in the description field of the DESCRIPTION file: the blog <https://globxblog.github.io/>, the presentation by Renard and Le Bescond (2022, <https://hal.science/hal-03710340v1>) and the poster by Renard et al. (2023, <https://hal.inrae.fr/hal-04388845v1>)
* As per email discussion of 09/01/2025, I kept using \dontrun{} in examples, but I added the following comment to tell the user why \dontrun{} is used and what to do to execute the example: "This line of code is wrapped in \dontrun{} since it relies on an external audio player to listen to the audio sample. See ?tuneR::setWavPlayer for setting a default player."
* I increased the package version from 1.0.0 to 1.0.1.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission
 
  Possibly misspelled words in DESCRIPTION:
    Bescond (10:144)
    Renard (10:130, 10:214)
    Sonification (3:36)
    al (10:224)
    et (10:221)
    sonify (10:37)
      
  All words are spelled correctly
