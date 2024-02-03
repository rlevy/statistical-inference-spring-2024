# README for 9.19 syllabus

## Prepping videos for YouTube

I believe that my videos are generally (all?) on Panopto; I would like to get them onto Youtube. Process for this:

* download video as mp4
* run `ffmpeg -i <filename> <output_name>.srt`
* run `python ~/9.19-dropbox/lectures/clean_panopto_captions_for_youtube.py < <output_name>.srt > <output_name1>.srt` to get rid of the font tags
* upload the mp4 video to YouTube
* re-add the captions within YouTube via `<output_name1>.srt` upload


## Updating syllabus

* Edit `assets/syllabus/syllabus.org`
* Run `python org_to_markdown_table.py` from the root directory
* to test on my laptop, run `conda activate jekyll` followed by `bundle install jekyll serve`
