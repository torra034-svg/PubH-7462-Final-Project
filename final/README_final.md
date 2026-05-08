# PubH-7462-Final-Project


1) The name of the project is Minnesota Geocache Finder.

2) This is a geocache treasure hunting app based in Minnesota. Users will use the app to entertain themselves by locating geocaches. 

3) The product can be accessed through shinyapps.io via this link:

https://54cdr7-patrick-torralba.shinyapps.io/Minnesota_Geocache_Finder/

4) We used geocache location data sources via opencache.us. Yes, this data source is publicly available.

https://www.opencaching.us/

5) The Shiny app displays an interactive map of Minnesota that shows geocache locations, a light mode / dark mode button, filter caches based on difficulty and terrain, a legend indicating cache type, and a feature that displays the 5 nearest caches to the user.

These features allow the user to determine which geocache they're interested in based on difficulty and terrain, and also lets the user navigate to a geocache close to them.

8) Interactivity [if applicable]: What are the main functions/actions that the app will allow the user to do?

The app allows users to interact with a map of Minnesota, click on specific geocaches, filter to geocaches of interest, and lets them adjust the light /dark mode setting.

Users can interact with the map by dragging their cursor around.

Specific geocaches can be accessed by clicking on them. This will provide coordinate information.

Geocaches can be filtered according to difficulty and terrain by using the slide bar to adjust to their preferences. And, the closest geocaches can be located when the user clicks anywhere within the state of Minnesota.

Dark mode can be enabled by clicking on the dark mode button.


9) Programming challenges: 

The main programming challenges we faced when implementing this app was creating the UI for the app. This was difficult because none of us were familiar with UI integration in R shiny apps, and we ended up utilizing AI to help us code UI features such as the difficulty bar.

10) Division of labor: 

Patrick was responsible for researching and importing geocache data into R, and project management aspects (e.g meeting times and due dates). Benny was responsible for the R Shiny app intergration (e.g server). And, Emmy was responsible for the UI section.