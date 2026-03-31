# PubH-7462-Final-Project


1) Name(s) of team member(s)

Benny Cox and Patrick Torralba

2) Product title

MN Hunt!

3) Product type (report, presentation, Shiny app, etc.)

Shiny App

4) Product purpose: what is the main purpose of your app?

This is a geocache treasure hunting app based in Minnesota. Users will use the app to entertain themselves by uploading geocaches they've found and/or uploading geocaches for others to find. 

5) What data source(s) will you be using, if any? Are they publicly available? If not, indicate why not, and if it will be possible to make your product publicly available after the end of the class. Again, it is okay if the data cannot be shared, but please document that here.

We will be using Minnesota geodata and publicly available photos. 

6) Product features: What are the main elements of your data product (tables, plots, etc.)? What questions will they answer and how will they provide insight into your data?

The Shiny app will display an interactive map of Minnesota displaying landmarks and tourist destinations. It will also display the number of people who've located the geocache / landmark. 

Users will try and determine which geocaches they're interested either through proximity, challenge level, and/or recently uploaded caches.

7) Automation [if applicable]: How will you automate the creation and deployment of your data product? What event(s) will trigger updates?

We will automate the app refreshing said app whenever a geocache has been uploaded or found. 


8) Interactivity [if applicable]: What are the main functions/actions that the app will allow the user to do?
NOTE: For items 5-8, It is OK to be "ambitious" and describe features and functionality that you are not 100% sure you will be able to implement. We would rather that you try to push your R programming limits (even if, in the end, you can't complete everything
you planned) than be overly safe and stick to things you already know how to do.

The app allows users to interact with a map of Minnesota, click on specific geocaches / landmarks, read a small fun-fact that's been scrapped from the internet (e.g wikipedia), and select which geocache they're interested in.

9) Programming challenges: What are the main programming challenges that you anticipate facing in implementing your data product?

The main programming challenge is creating an interactive map, data-scrapping the internet for landmarks and images, and creating a dynamic app that refreshes when triggers are set off (e.g geocache found or uploaded).

10) Division of labor: How will your team collaborate to complete the project? Will each person be responsible for a particular set of features, or will team members work together on the entire codebase? If you are working alone this part will, of course, be trivial.

Team members will collaborate on the coding process. Specifically, Patrick will code the mapping portion and Benny will scrape data from the internet for the R app. Then both individuals will work together to generate the R Shiny App.