---
title: "Megaplots Tutorial"
output:
  html_document:
    self_contained: true
    toc: true
    toc_float: true
    toc_depth: 3
    theme: flatly
---

## Introduction

A ‘megaplot’ is a ‘shiny’ application and as the name suggests, is a
huge graphical display showing individual-level data over time
interactively. In the context of clinical trials, megaplots seek to
represent longitudinal data while focusing on event visualization for
each subject throughout the entire course of the trial.

The concept of the megaplots app has been re-designed at the end of
2025. If you want to use the previous version, please use ‘Release
version v.1.1.10’.


## Megaplots Data
To make use of the app, a dataset must be uploaded as the first step.
Currently only .RData files are supported. Use the "Choose RData file" - Browse button
on the "File & variable selection"-tab to upload your file.

<img src="1_Megaplots.png" align="center" width="95%"/>

Once uploaded successfully, the Variable Selection boxes will appear. 

<img src="4_Megaplots.png" align="center" width="95%"/>

Autodetection for column names is implemented. If all required variables are 
selected, a "NEXT" button will appear.

### Required variables
The table below lists all recognised column names and their expected formats.
Column names are flexible - the app will ask you to map your own column names to the
required roles after upload.

| Column | Class | Note |
|:---|:---|:---|
| subjectid \* | numeric/character | unique subject identifier |
| start_time \*\* | integer (numeric) | numeric variables are rounded down to integer |
| end_time\*\* | integer (numeric) | numeric variables are rounded down to integer |
| event\* | character (numeric) | numeric variables are transformed to character |
| event_group | character (numeric) | if no selection is made, event is also used for event group |
| event_time \*\* | integer (numeric) | numeric variables are rounded down to integer |
| event_time_end \*\* | integer (numeric) | numeric variables are rounded down to integer |

\* mandatory variable (name can differ)

\*\* one of the variable pairs start/end_time or event_time/\_end is
mandatory (names can differ)

The variable ‘Identifier’ can be a numeric or character variable and
will be used to assign individual time sequences. The variables
‘Timeline Start Day’ and ‘Timeline End Day’ define the start and end
time of every individual time course. Both variables should be integers,
otherwise they will be rounded. ‘Event’ and ‘Event Group’ should contain
the names of the events that are displayed. Multiple events can belong
to a specific event group. If no event group is selected, event is
automatically set as event group. The event names do not necessarily
have to be unique. For display purposes, they are renamed if non-unique,
by writing the event group in brackets after them. Like the time
courses, every event requires a start and end day. The corresponding
variables can be selected via ‘Event Start Day’ and ‘Event End Day’. The
same rounding procedure is applied as for the timelines.

In every variable selection drop-down menu there is a ‘Clear’ button to
delete the selected variable. When all variables are in the desired a
format a next button appears in the lower half of the screen, which
leads to the next panel the ‘Event & color selection’.

### Time Variables

The app accepts two alternative ways to specify the time axis:

- start_time and end_time defines the overall observation window for each
subject (e.g. study entry and study exit).
- event_time and event_time_end could also define start and end of a subject.

At least one of these pairs must be present in the dataset. If both are 
provided, both will be used.

### Additional Variables

Any number of extra columns can be included alongside the required ones:

- **Numeric variables** - available for **sorting** subjects along the 
y-axis (e.g.sort by age or baseline value).
- **Character variables** -  available for **grouping** subjects into 
 separated blocks (e.g. group by treatment  arm or site).
 
For more information about how to use these variables interactively, refer to 
the **Sidebar Options**-chapter.

### Data Format Example

A minimal valid dataset would look like this:

| subjectid | start_time | end_time | event | event_group | event_time | event_time_end |
|:---|---:|---:|:---|:---|:---|:---|
| 001 | 1 | 365 | Headache | Other Symptoms | 10| 25 |
| 001 | 1 | 365 | Vomitting | Gastrointestinal Disorders | 30 | 34 |
| 002 | 1 | 280 | Nausea | Gastrointestinal Disorders | 48 | 48 |
| 003 | 14 | 119 | Death | Death| 119 | 119 |


## Event & Color Selection

In this panel, the events that should be displayed can be selected.
Events can also be sorted and all event colors can be defined. All
settings can be saved so that they can be quickly reloaded in future
sessions. First, the events are selected on the left side.

### Event / Event Group Tree

On the left side appears a list with checkboxes for all event groups.
Clicking on a box selects all events of the event group. The small
arrows next to the box can be used to expand the list. Then also every
event appears in the list.

<img src="7_Megaplots.png" align="center" width="95%"/>

This can be used to select only individual events of a event group. To
find individual events or event groups in a long list, the search box
above the list can be used. For every selected event a colored box
appears right to the event selection list. These can be used to set the
colors (see next chapter).

Another feature is the ‘drag and drop’ of the events within the list.
This can be used to sort events within a event group. This can be
particularly helpful for ordinal scaled events. Sorting affects the
legend displayed next to the graph but can also be used to create a
color palette explained in the next chapter. After sorting, any box of
the event selection list must be clicked again to update the color
selection. Further known issues with the drag & drop feature are that no
events may be selected in order to move variables and events can be
dragged out of the event group, which leads to errors.

After completing the event selection, it is possible to go directly to
the megaplot graphic by clicking the next button, or make various color
settings, which are explained in the next section.

### Event / Event Group Color List

In the middle of the ‘Event & color selection’-panel, a colored list
with all selected event groups and events appears. It is now possible to
mark an event group or a single event by clicking on the corresponding
event/event group name. The selected event or event group gets a
highlighted border.

<img src="10_Megaplots.png" align="center" width="95%"/>

By clicking one of these events a color selection panel on the right
side appears. Depending on whether a single event or a event group is
clicked, the options in the color selection panel differ.

Note: If you change the event selection on the left side after making a
selection for the colors in the middle, it may happen that a event/event
group selection does not respond. By clicking any other event in the
color list updates the selection and resolve the issue.

### Color Selection Panel

After selecting an event or event group in the color list, options to
change the colors appear on the right side. There is also an jitter
option to adjust the event position of the event group, which will be
explained in the chapter after next.

### Color Methods

Selecting suitable colors is particularly difficult when many different
events are to be represented. The user is responsible for selecting
suitable colors and take color blindness into account. For the correct
color selection, please refer to the various best practice guides. The
methods here are only intended as an aid to their implementation.

When an event group is selected, there are three different methods to
colorize the events: ‘Color gradient (3 colors)’, ‘Unique color for all
events withing group’ and ‘Distinct color by selected palette’.

<img src="11_Megaplots.png" align="center" width="95%"/>

When using color gradient, three color inputs appear which will define a
color palette displayed below. In this example, the colors yellow, gray
and turquoise are used. Depending on how many events are there within an
event group, there are that many colors, created by these three colors.
By clicking one of these color inputs a color palette appears which can
be used to change the colors and their saturation. However, it is also
possible to directly access a color hexcode into the color box. Once a
satisfactory color palette has been found, click the ‘Update colors’
button to apply the changes.

The second option ‘Unique color for all events within group’ obviously
colors all events of an event group in the same color. So there will be
exactly one color input. Here too, the color can be selected using the
palette or by entering the hexcode.

The third option ‘Distinct color by selected palette’ brings up another
drop-down menu ‘Select color palette’ where predefined color palettes
can be selected. By selecting one of these palettes a good visulization
is not guaranteed and must be ensured by the user.

If an single event is selected in the color list the color selection on
the right will be the same for ‘Unique color for all events within
group’ explained above. So it is possible to first create a color
palette for the complete event group and after that change individual
colors by selection the event.
<img src="14_Megaplots.png" align="center" width="95%"/>

### Offset Events

In order to better distinguish events in addition to color, they are
also slightly offset in height by default. To prevent this, the check
mark can be removed for option “Offset events for event group”.

<img src="18_Megaplots.png" align="center" width="50%"/>

The offset position will be the same for each time course. An offset
position for events within an event group only makes sense if different
events at a given day can appear. For unique events within a event
group, it is recommended to disable this option.

### Save & Upload Color Files

Once all color settings have been made, they can be saved via the ‘save
color file’-button.
<img src="19_Megaplots.png" align="center" width="20%"/> This saves a
data frame as ‘.rds’-file including the selected event and event group,
the selected color and if available all three selected gradient color
and also the logical value if variables should be offset.

To upload the saved color settings in a new session use the ‘Upload
saved color file’-file input like the data upload.

## Megaplots

In this chapter the main graphical display will be explained. The
megaplot is a huge graphical display used to show individual-level data
over time. The megaplots uses horizontal lines to represent individual
trajectories and events of any identifier over days (in clinical trial
context: study units).

<img src="23_Megaplots.png" align="center" width="95%"/>

The legend on the right side can be used to select and deselect event
groups to provide an better overview. Selecting a high number of events
can lead to overlaying event lines. In this case it is recommended to
focus on a few number of event/event groups or zoom-in on the graphic
accordingly.

For every single event displayed a hover panel is available with
information about the identifier, the event name as well as the start
and end time. It is also possible to use the mouse hover for the
individual timelines, however these are only visible when hovering near
the start or end of the line.

<img src="25_Megaplots.png" align="center" width="95%"/>

In addition to hovering, you can also click on event lines. The complete
clicked event are then highlighted in comparison to other events. It is
possible to click on and highlight as many events as desired. To undo
the effect, double-click on the plot window.

<img src="27_Megaplots.png" align="center" width="95%"/>

There are several ways to zoom-in to the graphic. First, it is possible
to use mouse-scrolling to zoom-in. To re-scale only one axis, click and
drag near the edge of one of the axes. If the drag mode is set to ‘pan’
(see chapter Plot options) it is possible to click and drag on the plot.
Another zoom possibility is to click and drag on the plot when option
‘Zoom’ is selected.

<img src="28_Megaplots.png" align="center" width="95%"/>

Third way to zoom is to use the ‘plus’ or ‘minus’ button on the modebar
to zoom-in or zoom-out.

Further options for this megaplots graph are described in the chapter
“Sidebar Options”, such as sorting, grouping or line thickness.

## Event Summary

In addition to the long-proven megaplots graph, we have included 
another plot in to most recent version, the so-called **Event Summary**.
This prodcuces an interactive line chart that visualize how clinical
trial events are distributed across study days. Each event type is 
rendered as a separate line, making it straightforward to judge whether
a particular event tends to cluster early or late in a study. 
The chart is also built with plotly, so it supports interactive hover label
that display exact event counts for any given day. There is also an
interactive legend, to add or remove events in a snap.
This makes it easy to track the number of events up to a specific day.
 
In the picture below a Event Summary for all kinds of events are displayed. 
Depending on how often the events take place, it may make sense to look at 
specific event groups one by one, especially when mixing common daily events with 
rare ones.
<img src="30_Megaplots.png" align="center" width="100%"/> 

The hover functionality now works as follows: You can select a data point along
the x-axis, an a vertical dashed line will appear at that point. 
For each event line, one small label appears, showing the corresponding event count
for that day. In the plot appearance option, you can set the threshold for when
events should be displayed to minimize the number of labels if there are too 
many events.

<img src="31_Megaplots.png" align="center" width="100%"/> 

Another option is to generate one hover label with all events and event counts 
listed in a same way the legend is created. This option is also available in 
the sidebar at the Plot appearance options.

### Summary display
Above the chart is an Dropdown menu called 'Select summary display'.
The user has the option to select out of three options:


- **Number of events per day** -	Shows the raw count of event occurrences on each individual study day.
- **Number of events per day (cumulative total)** - Shows the running total of all event occurrences accumulated from the start of the study.
- **Number of first events per day and subject (cumulative total)**-	Like (cumulative total), but recurring events for the same subject are deduplicated — only the first occurrence per subject per event type is counted.
The default is **Number of events per day**, which is the most suitable starting point for spotting temporal clustering.


<img src="32_Megaplots.png" align="center" width="100%"/>  

As for the Megaplot chart, there is also the option to group the **Event Summary** by
one or multiple variables. This feature makes it easy to compare different
groups. Unfortunately, hovering and zooming only works for one group at a time. 
Also keep in mind, that y-axes may differ after zooming in one grouped graph.

<!-- <img src="33_Megaplots.png" align="center" width="100%"/>  --> 

<!--<img src="34_Megaplots.png" align="center" width="100%"/> -->


## Sidebar Options

Various options are available on the sidebar, which would be
‘Sorting/Grouping’, ‘Plot appearance’, ‘Filter’ and ‘Download’. Each of
these option panels is explained in a separate chapter below.

### Sorting / Grouping

When you click on ‘Sorting/Grouping’, a panel opens with the three
inputs: ‘Sorting variable’, ‘Grouping variable’ and ‘Arrange Groups’. In
the sorting variable input, every numeric variable of the uploaded data
set is eligible. Therefore custom sorting variables can be created in
the data preparation.

All character variables in the upload data can be used as a grouping variable.
If a grouping variable is selected, the time-courses are splitted by all
variable values.

<img src="36_Megaplots.png" align="center" width="95%"/>

It is also possibly to select multiple grouping variables. Then the
option ‘Arrange groups’ might be of interest, as it allows the groups to
be reordered. This can be done simply by drag and drop the
displayed group boxes.

<img src="38_Megaplots.png" align="center" width="100%"/>

<!-- <img src="39_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="40_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="41_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="42_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="43_Megaplots.png" align="center" width="100%"/>  -->



### Plot Appearance

A wide variety of options related to appearance can be found here.
Changing one of the option in the **Plot appearance** panel re-renders the graphs.
For charts with a large number of events and high render times,and if you need 
to change multiple options, it may be faster to first deselect all events.

The first one is a drag and drop feature to re-arrange the order of which 
the event lines are drawn. It is strongly recommended to sort rare or important
events to the bottom of this list to ensure they remain visible.

The next feature is an color option to change the color and/or its transparency
of the subject line. Depending on which theme is selected at the top right corner,
the user has to change the corresponding color.

The width of the subject time line as well as the width of the events lines can 
be changed via slider.

The toggle switch for **Legend Grouping** determines, whether entire event groups
are selected in the legend or only individual events. The appropriate option 
depends on the specific situation. If events of groups events are only display 
together and need to be deselected quickly, **Legend Grouping** is the better choice.
If individual events are to be selected, this option can be turned off to compare
individual events with one another.

The two remaining options are for the **Event Summary** chart and are explained there.
The first one changed the hover label appearance and the second one determines for
which treshold, the labels should appear.

### Reference Lines/Rectangles

In the older megaplot version, there was an option to draw reference lines into 
the megaplot chart.
We have adopted this option and are expanding on it in the sense that it is now
also possible to draw rectangles in the background. 
The setup is simple.
Simply check "Add reference rectangle", select a color (+transparancy), select
an x-axis start value (x1) and an x-axis end value (x2). If the same number for
x1 and x2 is entered, a vertical line at this specific day will be drawn. 
If different values are entered, then a rectangle is drawn in the background. 
So far, up to 3 different lines or background rectangles can be drawn. 
This can therefore serve as a helpful visual reference for defining specific time periods
or marking an important day for analysis.
<!-- <img src="44_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="45_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="46_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="47_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="48_Megaplots.png" align="center" width="100%"/>  -->


### Filter

The filter panel uses the datamods package which provides custom modules and one of them
is to produce the filters. This makes it easy to filter your data set fast and simple.

With the selection box **Select filter variable(s)** all variables
can be selected and for the selected variables further options will appear.
Every selected numeric variable will create a slider and every character
variable with create a selection box. If the variables includes missing values
(NAs), there is also a little switch to include or exclude NAs for the variable.
After all filter selection where made, the charts updating with the 
reduced/filtered data set.
<!-- <img src="49_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="50_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="51_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="53_Megaplots.png" align="center" width="100%"/>  -->

### HTML Download

<!-- <img src="54_Megaplots.png" align="center" width="100%"/>  -->

This panel currently contains exactly one button "Download Mega plot as HTML".
So when you click it, the megaplot chart can be saved in HTML format.
All plotly options are preserved in this file, but app options like sorting
and grouping are of course not available. 

<!-- <img src="55_Megaplots.png" align="center" width="100%"/>  -->

<!-- Explanatory text for this chapter will follow soon. -->

### Sequencing 

## Additional information

<!-- <img src="56_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="57_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="58_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="59_Megaplots.png" align="center" width="100%"/>  -->

<!-- <img src="60_Megaplots.png" align="center" width="100%"/>  -->

To use the full screen size for your megaplot hide sidebar, if not used.
