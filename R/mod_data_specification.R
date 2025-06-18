#' Modul Data Specification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @keywords internal

mod_data_specification_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shinydashboard::box(
      width = NULL,
      solidHeader = TRUE,
      collapsible = FALSE,
    shiny::uiOutput(ns("data_specification"))
    )
  )
}

#' Modul Data Specification Server Function
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#'
#' @noRd
#' @keywords internal

mod_data_specification_server <- function(input, output, session, select_color) {
  ns <- session$ns
  output$data_specification <- shiny::renderUI({
    list(
      HTML(
      paste0("
      <h2 style = 'color: ",select_color()['plot.id'],";'> megaplots Package Manual (", packageVersion("megaplots"),")</h2>

      <h5 style = 'color: ",select_color()['plot.id'],";'> Madhurima Majumder, Susanne Lippert, Bastian Becker,
      Steffen Jeske, Svenja Jedhoff, Silke Janitza, Hermann Kulmann </h5>

      <h3 style = 'color: ",select_color()['plot.id'],";'> February 7th, 2024 </h3>
       <p style = 'color: ",select_color()['plot.id'],";'>
        Table of content:
      </p>
        <ul>
          <li> <a href='#chap1'> 1. Description </a></li>
          <li> <a href='#chap2'> 2. Functionality </a></li>
          <li> <a href='#chap3'> 3. Data specification </a></li>
          <li> <a href='#chap4'> 4. Additional information </a></li>
        </ul>

        <div id='chap1'>
        <h3 style = 'color: ",select_color()['plot.id'],";'> 1. Description </h3>
        <p style = 'color: ",select_color()['plot.id'],";'>
          A 'megaplot' is a 'shiny' application and as the name suggests,
          is a huge graphical display showing individual-level data over time
          interactively. In the context of clinical trials, megaplots seek to
          represent longitudinal data while focusing on event visualization
          for each subject throughout the entire course of the trial.
          It also implements artificial intelligence techniques to produce
          a 'smart' plot, thus enabling the user to detect interesting patterns
          in the data across the entire population or within specified subgroups.
          With this, one can get a deeper insight into the data in ways that
          are not possible with traditional static data visualizations.
        </p>

        <div id='chap2'>
        <h3 style = 'color: ",select_color()['plot.id'],";'> 2. Functionality </h3>
          <p style = 'color: ",select_color()['plot.id'],";'>
          Once the 'megaplots' package is installed, simply call this
          application through the function launch_megaplots().
          Next, upload the subject-defining and event-defining
          datasets through the 'Data Import (Upload Data)'
          panel. Alternatively, a previously created and saved
          megaplot may also be uploaded through the 'Data Import
          (Upload saved data)' panel. For more information on the required
          data structure see Data Specification (Section 3) below. If it
          is of interest to sort the graphical display according to the order
          of events, it is possible to select 'Sort events' and further
          'Select order of event' in the same 'Data import' tab.
          Additionally, the screen format can also be specified here
          for optimal display. Once you hit 'Submit', it takes you to
          the 'Megaplot' panel which is the actual graphical display of
          all events for all subjects, with the horizontal axis showing
          the time frame and the vertical axis showing the subject IDs.
          The horizontal lines against the subject IDs show when and which
          event has occurred, including the event level and multiple events
          on the same day. The 'Main options' panel on the top helps navigate
          this plot interactively, e.g., selecting/deselecting events and their
          levels according to the user's interest; selecting grouping and
          sorting variables (e.g., by subject ID, by default). Grouping
          and/or sorting help in detecting patterns among subjects and events.
          There is also a reference line that by default is set at
          'time zero' and may be changed as needed. Finally, the Zoom
          feature helps to zoom in any portion of the plot to read it in
          greater detail. The next panel is for 'Displayed subjects' which
          is a feature to select subsets of patients, either deterministically
          or randomly, for display in the plot. The app also offers artificial
          intelligence features through the 'Artificial Intelligence'
          panel sequencing of variables for sorting, respectively.
          The 'Settings' panel has
          several options for display, including thickness of subject
          lines, color theme etc. The color palette to depict different
          events/different levels of the same event is available through
          the 'Color Options' panel. Additionally, the raw data as well as
          summary statistics (e.g. number of events for each subject) for
          the data displayed in the plot may be accessed through the
          corresponding panels.
           </p>
        <div id='chap3'>
        <h3 style = 'color: ",select_color()['plot.id'],";'> 3. Data specification </h3>
          <p style = 'color: ",select_color()['plot.id'],";'>
            Two datasets are to be created: The subject-defining dataset (A)
            contains one record per subject. The event defining dataset (B)
            has one record for each relative day per subject on which the
            subject experienced at least one of the prespecified events.
            Both datasets have mandatory variables with prescribed names
            and format as described below:
            <li style = 'color: ",select_color()['plot.id'],";'>
              A: subjectid, start_time, end_time
            </li>
            <li style = 'color: ",select_color()['plot.id'],";'>
            B: subjectid, ae_time
            </li>
            <p style = 'color: ",select_color()['plot.id'],";'>
            Missing values in mandatory variables are not allowed.
            The relative day event_time in dataset B can be based on any date
            at the beginning of the individual study course (e.g., screening
            date, randomization date, or first drug intake). There is no
            prespecified order for variables in the datasets. The app
            puts the data in the correct order. The app supports two
            file formats of data to build a new megaplot ('Upload data'):
            </p>
            <li style = 'color: ",select_color()['plot.id'],";'>
              Comma separated values (CSV) file
            </li>
            <li style = 'color: ",select_color()['plot.id'],";'>
              RData file
            </li>
            <p style = 'color: ",select_color()['plot.id'],";'>
            The input datasets A and B  CSV or RData file for input must
            include a data frame with the following variables and formats:
            </p>
          </p>
          <style style = 'color: ",select_color()['plot.id'],";'>
            table {
              font-family: arial, sans-serif;
              border-collapse: collapse;
              width: 100%;
            }
            table {
              width: 100px;
            }
            td, th {
              border: 1px solid ", '#424242', ";
              text-align: left;
              padding: 8px;
            }
            tr:nth-child(odd) {
              background-color: ", '#e3e3e3', ";
            }
            tr:nth-child(even) {
              background-color: ", '#e0e0e0', ";
            }
          </style>

          <table>
            <tr>
              <th>   </th>
              <th> Dataset </th>
              <th> Mandatory variables </th>
              <th> Column </th>
              <th> Specified </th>
              <th> Example </th>
            </tr>
            <tr>
              <td rowspan='8'> Sorting/Grouping Factors <br>
                   <em> One record per subject </em>
              </td>
              <td rowspan='8'> A
              </td>

              <td>
               x
              </td>
              <td>
                subjectid
              </td>
              <td>
                int
              </td>

              <td>
              </td>
            </tr>
            <tr>
              <td> x </td>
              <td> start_time </td>
              <td> int </td>
              <td> -18 (i.e. 18d in screening)</td>
            </tr>
            <tr>
              <td> x </td>
              <td> end_time </td>
              <td> int </td>
              <td> 291 (i.e. 291d documented after 1st dose)</td>
            </tr>
             <tr>
              <td>  </td>
              <td> sex </td>
              <td> chr </td>
              <td> </td>
            </tr>
             <tr>
              <td>  </td>
              <td> age_class </td>
              <td> chr </td>
              <td> </td>
            </tr>
             <tr>
              <td>  </td>
              <td> height_class </td>
              <td> chr </td>
              <td> </td>
            </tr>
             <tr>
              <td>  </td>
              <td> region </td>
              <td> chr </td>
              <td> </td>
            </tr>
             <tr>
              <td>  </td>
              <td> treat_df </td>
              <td> chr </td>
              <td> </td>
            </tr>


           <tr>
              <td rowspan='6'> Events per study-day <br>
                   <em> One record per relative day - if at least one event observed out of ae1, ae2, ae3,..., aeX </em>
              </td>
              <td rowspan='6'> B
              </td>

              <td>
               x
              </td>
              <td>
                subjectid
              </td>
              <td>
                int
              </td>

              <td>
              </td>
            </tr>
            <tr>
              <td> x </td>
              <td> event_time </td>
              <td> int </td>
              <td> Relative day</td>
            </tr>
            <tr>
              <td>  </td>
              <td> ae1 </td>
              <td> chr </td>
              <td> Pain=MILD </td>
            </tr>
              <tr>
              <td>  </td>
              <td> ae2 </td>
              <td> chr </td>
              <td> Biopsy=YES </td>
            </tr>
              <tr>
              <td>  </td>
              <td> ae3 </td>
              <td> chr </td>
              <td> Treat=ACTIVE 2MG </td>
            </tr>
             <tr>
              <td>  </td>
              <td> aeX </td>
              <td> chr </td>
              <td> </td>
            </tr>


          </table>

        <p style = 'color: ",select_color()['plot.id'],";'>
        Besides baseline characteristics for grouping and sorting,
        you can also add post-baseline characteristics in dataset A.
        For instance, the number of days with severe pain could serve as
        sorting variable, or, if classified (<1 day, 2-3 days, >=4 day),
        you can use it for grouping.

        Note for RData input file:
        All sorting/grouping factors (A) and event-outcomes (B) need to be
        in character ('chr') format.
        There are two options for uploading RData files:
        <li style = 'color: ",select_color()['plot.id'],";'>
          Two separate RData files containing data frame A and B, respectively, can be uploaded.
        </li>
        <li style = 'color: ",select_color()['plot.id'],";'>
          One RData file which is a list object with elements A and B, can be uploaded.
        </li>
        <p style = 'color: ",select_color()['plot.id'],";'>
        Furthermore, an upload of an existing megaplot with all settings is
        possible ('Upload saved data'). This requires that a
        previously created megaplot has been saved as an rds-file via
        'Save Session Settings' (navigation pane 'Main option').

        </p>

        <div id='chap4'>
        <h3 style = 'color: ",select_color()['plot.id'],";'> 4. Additional information</h3>

        <p style = 'color: ",select_color()['plot.id'],";'>
        <li style = 'color: ",select_color()['plot.id'],";'>
          The display of the observation time (x-axis) is limited. If the maximum number of days is high - then check if the event symbols are still displayed correctly. If not - reduce the maximum (via zoom-in) or decrease the number of displayed events and/or event-levels. Use the scroll function under Zoom (navigation pane 'Main option') to 'screen' through the whole time period.
        </li>
        <li style = 'color: ",select_color()['plot.id'],";'>
          To use the full screen size for your megaplot: a) Press F11 before pressing Submit button in Data Import, b) Hide both navigation panes, if not used.
        <li style = 'color: ",select_color()['plot.id'],";'> You can save a megaplot with all the settings, via Save Session
        Settings (navigation pane 'Main option'). If it does not work
        change your browser.
        </li>
        <li style = 'color: ",select_color()['plot.id'],";'>
          The number of events displayed in a megaplot is limited to 4. If dataset B contains more events - the app takes the first 4 events automatically. These can be manually changed under Data import (left navigation pane), before pressing the Submit button.
        </li>
        <li style = 'color: ",select_color()['plot.id'],";'>
          For a detailed look use the Open/Close Zoom Panel (navigation pane 'Main option') to provide Zoom-in screenshot. The magnifier (Windows) can help, too. If the megaplot remains empty after uploading your data - make sure that all values are checked under Select event levels (navigation pane 'Main option').
        </li>
         </p>
        ")
      )
    )
  })
}

