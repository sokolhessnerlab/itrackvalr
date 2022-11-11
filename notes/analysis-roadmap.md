# Roadmap for CSN analysis

## Ari's To-Do Items

### Administrative

- [ ] Get Ari's `@du.edu` email address back.
- [ ] Test S drive access; if none, ask Peter to arrange for drive access.
- [ ] Inform Princeton's IRB about project at DU.

### Technical

- [ ] Upgrade `itrackvalr` package dependencies, especially `targets` and `tidyverse` ([issue #26](https://github.com/sokolhessnerlab/itrackvalr/issues/26)).
- [ ] Check that analysis pipeline produces the same results with upgraded packages.

### Analysis: Behavior

- [ ] Run remaining regressions (see [issue #25](https://github.com/sokolhessnerlab/itrackvalr/issues/25)).
- [ ] Update documents summarizing behavioral analysis.
- [ ] Review behavioral analysis to confirm all done.

### Analysis: Eyetracking

#### Preparation

##### Draw inferences from validation data
- [ ] Examine relationship between validation errors pre-task and post-task (correlated? sig. diff?).
- [ ] Calculate and examine size of implied 'zone of uncertainty' in gaze location based on mean errors from validation (extract: radius of zone).
- [ ] Identify _x_ & _y_ translational offsets to use (pre-task? linear interp between pre- and post-task?).

##### Prepare for upsampling and standardizing eye tracking data
- [ ] Identify temporal resolution for upsampling and standardizing eye tracking data.
- [ ] Identify location of the clock hand's tip at every moment in time in upsampled/standardized data on a per-participant basis.

##### Do dimensionality reduction on eye tracking data
- [ ] Upsample/interpolate/standardize eye gaze data on a per-participant basis.
- [ ] Calculate distance between eye gaze sample and clock hand's tip.
- [ ] Use radius of 'zone of uncertainty' to binarize upsampled eyegaze data into _on-task_ (1; distance of gaze loc from clock hand tip is < radius of the zone of uncertainty) and _off-task_ (0; distance is > radius of zone of uncertainty).
- [ ] Save out binarized data for all further analysis.

##### Preparation of data on image content
- [ ] Review status of image content analyses (to allow categorization of images and their content for use in analyzing gaze location with respect to images).
- [ ] Get all image content information into uniform data structure (incl. type, ratings, attributes, as applicable).

#### Model-Free Analyses of binarized data
- [ ] Segment binarized gaze data into ~1 second bins corresponding to single image presentations on the screen ("trials").
- [ ] Calculate p(_on-task_) for the average second (graph should be 0-1s on x, p(_on-task_) on the y; 3600 "trials" being averaged together)
- [ ] Calculate p(_on-task_) for different _types_ of images and image content (i.e. high-arousing, med-arousing, low-arousing).

#### Model-based analyses of binarized data
- [ ] Review model code from CSC analysis.
- [ ] Integrate relevant image content information gleaned from model-free analyses into model(s).
