#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(deSolve)
library(reshape)
library(ggplot2)
library(DT)


### load global data
# for reproducability
#random_seed <- 124
#options(shiny.maxRequestSize = 5*1024^2)

#fileName = "intro_description.txt"
#intro_description <- readLines(fileName, file.info(fileName)$size)
#fileName2 = "intro_description2.txt"
#intro_description2 <- readLines(fileName2, file.info(fileName2)$size)

WA_cases <- read.csv("WA_cases.csv")
# re-sort it
WA_cases <- WA_cases[order(as.Date(WA_cases$date, "%m/%d/%Y")),]
default_popn <- 4  # million for King, Pierce, and Snohomish counties
default_severe_symptomatic <- 0.05
default_deaths <- 0.01

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("spacelab"),
                
    img(src="DEOHS-Logo-Line-Purple-Print-Transparency.png", height=25, style="margin-top:10px"),
    HTML("<hr>"),
    #TitlePanel#############################################################
    titlePanel("Washington State Coronavirus Epidemiologic Outbreak Modeling"),
    ########################################################################
#    img(src="SoulCatcher-Original.png", height=40, style="float:right; margin-right: 100px; margin-top: 15px"),
    HTML("<div style='color:lightgray'>COVID-19 model; SARS-CoV2 model</div>"),
    HTML("Edmund Seto &lt;eseto@uw.edu&gt;<br>Associate Professor, School of Public Health, University of Washington<br>version 1.0"),

    navlistPanel(
        #Introduction############################################################
        "Introduction",
        ########################################################################
        
        tabPanel("Infectious disease models to inform disease control policies",
            h1("Getting Ahead of the Curve"),
            HTML("<br>"),
            img(src = "inslee_3_10_2020.png", height = 200, style="float:left; margin:10px"),
            HTML("<br><br><br>"),
            includeHTML("intro_description1.html")
        ),
        
        tabPanel("How do models work?",
            h1("SIR Epidemiological Model"),
            HTML("<br>"),
            img(src= "800px-Ronald_Ross.jpg", height= 200, style="float:left; margin:10px"),
            HTML("<br><br><br>"),
            includeHTML("intro_description2.html")
        ),
        
        tabPanel("Interactive models: Go ahead, twist knobs!",
                 h1("Interactive Models to Address Policy Questions"),
                 HTML("<br>"),
                 img(src="bruce_aylward.png", height=200),
                 HTML("<br>"),
                 includeHTML("intro_description3.html")
        ),
        
              
        
        
        #Model Specification####################################################
        "Answers to Key Questions",
        ########################################################################

        #Question 1###################################################
        tabPanel("1. What does the situation look like in WA state?",
        h1("Washington State Case Data"),
        HTML("<br>"),
        img(src="inslee_3_10.png", height=144, style="float:left; margin:10px"),
        HTML("<br><br><em>If there are a 1000 people infected today, in 7 or 8 weeks there could be 64,000 people infected in the state of Washington if we don’t somehow
             slow down this epidemic. And the next week it’d be 120,000. And the next week it’d be a quarter million. -- Governor Jay Inslee, 3/10/20.</em><br style='clear:both'><br>"),
        
        HTML("Historical data on cases and deaths were not easy to find. The <a href='https://www.doh.wa.gov/Emergencies/Coronavirus'>
        WA State Department of Health's
             website</a> only lists current cases and deaths.  I used Internet archives of their website to reconstruct the history of cases and deaths in the state, up to March 20, 2020.
             Note that cases identified and confirmed are dependent on the amount of testing for cases. Testing hasn't
             been widely available, and availability has changed over time.  These reported cases are likely
             the 'tip-of-the-iceberg' with respect to the virus infections in the the community because of limited availability of 
             testing, some cases might be very mild, and not everyone with infection gets it confirmed with testing. 
             <br><br>Just found: Washington State Hospital Association
             seems to maintain an <a href='https://www.wsha.org/for-patients/coronavirus/coronavirus-tracker/'>updated case tracker</a>."),
        
        plotOutput("WA_casesPlot"),
        
        HTML("<br>Below is an illustration of the number of deaths reported in the state."),
        plotOutput("WA_deathsPlot"),
        
        HTML("<br>Here's the data, if you're interested in it:"),
        #HTML("<div style='color: black'>"),
        dataTableOutput("WA_casesData")
        #HTML("</div>")
        ),
        
        #Question 2###################################################
        tabPanel("2. Can we fit a model to the WA state data?",
        h1("Model Parameter Fitting to Data"),
        HTML("<br>"),
        img(src="Jeff Duchin 3_11_2020.png",height=144, style="float:left; margin:10px"),
        HTML("<br><br><em>Presenting modeling results from Trevor Bedford, Fred Hutch: 'The blue line shows what will happen if we don’t 
        implement any social distancing or community mitigation measures to flatten that curve.'
             -- Jeff Duchin, Health Officer Seattle & King County, 3/11/2020</em><br style='clear:both'><br>"),
        
        HTML("We can fit a SIR model to the WA state COVID-19 cases. We'll have to make some assumptions
        though because we don't know exactly whether these cases, represent  people who had the worst
        symptoms, and whether there are hundreds or thousands of infected and infectious cases in the 
        community that were not identified and confirmed with testing.<br><br>
        Because of these uncertainties, we'll just be looking for a crude fit (to the proportion of cases
        reported up to Mar 20 for each of the preceding days), in which the rise
        in the number of infections in the model matches the shape of rise in proportion of cases.
             Note that the large majority (>80%) of cases and deaths are located in King, Snohomish, and Pierce
             Counties, which account for about 4 million persons of the state's 7.8 million population.<br><br>"),

        HTML("Effective Contact"),
        sliderInput("contact_rate_fit", "How many person contacts with an infectious person would result in a new infection each day? (default:4)",
                    min = 1, max = 20, value = 4
        ),
        
        HTML("Infectious Period"),
        sliderInput("infectious_period_fit", "After someone is infected, how may days are they infectious? (default:16)",
                    min = 1, max = 30, value = 16
        ),
        
        HTML("<strong>Goodness of Fit</strong><br>The RMSE is an indication of the how well the model fits the data:"),
        textOutput("fit_RMSE"),
        HTML("<br>Below, the bars are the WA state case data, and the blue line represents the model fit."),
        
        plotOutput("SIRplot_fit"),

        HTML("You can alter the sliders for the two parameters, but it looks like values of:<br>
             Effective Contact = 4<br>
             Infectious Period = 16<br>
             results in the best fit using the RMSE goodness of fit measure shown above the figure.<br><br>")
        
  #      HTML("Table of model fit stats:"),
  #      dataTableOutput("fit_RMSE_dt"),
        
        ),
        
        #Question 3###################################################
        tabPanel("3. If we do nothing, how many will be infected?",
        h1("Infections Without Any Intervention"),
        HTML("<br>"),
        img(src="tedros 3_5_2020.png", height=220, style="float:left; margin:10px"),
        HTML("<br><em>This is not a drill.<br>
            This is not the time to give up.<br>
            This is not a time for excuses.<br>
            This is a time for pulling out all the stops…<br><br>
            If we take the approach that there’s nothing we can do, that will quickly become a self-fulfilling prophecy.<br>
            It’s in our hands. -- Tedros Adhanom Ghebreyesus, WHO Director General, 3/5/2020</em><br style='clear:both'><br>"),
        
        HTML("Let's use the parameters that we found were the best fit to case data from the previous step:<br>
             Effective Contact = 4<br>
             Infectious Period = 16<br><br>
            Now, to estimate the number of infections that will occur from the outbreak, we also need to know how large is the 
            community we'd like to model. As described in the Washington state case data section,
            most cases and deaths have been in the 3-counties (King, Snohomish, and Pierce), and so we might
            want to focus on the approx 4 million in there rather than the entire 7.5 million in Washington (the
            contact rates will probably be different in more rural areas, and certainly there won't be complete
            mixing between counties, even under a normal non-outbreak situation).<br><br>"),

        HTML("Effective Contact"),
        sliderInput("contact_rate", "How many person contacts with an infectious person would result in a new infection each day? (default:4)",
                    min = 1, max = 20, value = 4
        ),
        HTML("Infectious Period"),
        sliderInput("infectious_period", "After someone is infected, how may days are they infectious? (default:16)",
                    min = 1, max = 30, value = 16
        ),
        HTML("Population Size"),
        sliderInput("population_size", "How many people are in the community (in millions)? (default:4)",
                    min = 1, max = 50, value = 4
        ),
        
        plotOutput("SIRplot"),
    
        
            HTML("<br>"),
        
        HTML("<strong>Infections</strong>"),
        textOutput("numberInfected"),
        HTML("<br>Using default settings we find that, in the absence of any interventions, nearly everyone 
             (98% or approx. 3.9 million people in the state will be infected. For the default
             settings, notice how the S curve drops all the way down to 2% -- that means that 100% - 2% = 98% of the population
             was infected at some point during the outbreak. At the 'peak' of infection (around April) around 38% of the 
             community is infected at the moment in time!<br><br>
             But keep in mind not all of these infections will result in symptoms, and many may not be discovered. 
             Moreover, this might be the case only if 'we do nothing' to stop virus spread.
             And some might argue that having a lot of asymptomatic individuals, and having the bulk of the community 
             move to the 'R' (Recovered) state may 
             actually be good for building up 'herd immunity' to potential future re-emergence of this virus. We will
             visit this issue later in the section on ('I hear people talk about R0 for the virus -- what's that about?' and
             'Why is a seroprevalence test even more important than a COVID-19 diagnostic test right now?').
             <br><br>")

        ),
  

        #Question 4###################################################
        tabPanel("4. But not all infections result in symptoms -- How many people will be sick? How many will die?",
            h1("Illnesses and Deaths in the Absence of Intervention"),
            HTML("<br>"),
            img(src="inslee_3_11_2020.png", height=144, style="float:left; margin:10px"),
            HTML("<br><br><em>This is not just your ordinary flu. This is a virus that the experts tell us is at least 10 times more 
            potentially fatal than the flu. -- Governor Jay Inslee, 3/11/20.</em><br style='clear:both'><br>"),
        
            HTML("<strong>Illnesses</strong><br>We don't know the true proportion of infected (infectious) individuals in the 'I' group that actually
            will have symptoms. If we assume a value for this, like 50% will have symptoms, and that the duration of symptoms is roughly
            the same as the duration of being infectious, then we simply multiply the number in the 'I' group at any
            given time by 50%.<br><br>
            The number of symptomatics is important because some will be sick enough that they'll need to seek 
            healthcare services. There is a 'peak' period in the outbreak when there will be large number of sick people, which
            may overrun available healthcare resources. We'll discuss how to 'flatten this curve' later.<br><br>
            Conversely, the number of people who do not get sick (the <strong>a</strong>symptomatic persons) are important because these 
            could be potentially 'silent carriers' and 'silent spreaders' of infection. Because they don't feel sick, they may still be
            active in the community, coming into contact with others to spread infections.<br>
            <br>
            Here's what symptomatics look like with some of the same default slider values as in the previous modeling step.<br><br>"),
        
            HTML("Effective Contact"),
            sliderInput("contact_rate_symptoms", "How many person contacts with an infectious person would result in a new infection each day? (default:4)",
                        min = 1, max = 20, value = 4
            ),
            HTML("Infectious Period"),
            sliderInput("infectious_period_symptoms", "After someone is infected, how may days are they infectious? (default:16)",
                        min = 1, max = 30, value = 16
            ),
            HTML("Population Size"),
            sliderInput("population_size_symptoms", "How many people are in the community (in millions)? (default:4)",
                        min = 1, max = 50, value = 4
            ),
            HTML("<br><br>"),
            HTML("Percent Symptomatic"),
            sliderInput("symptomatic_percent", "What percent of infected (infectious) persons have symptoms? (default:50)",
                        min = 1, max = 75, value = 50
            ),
            
            plotOutput("SIRplot_symptoms"),
            HTML("<br>"),
        
            HTML("<strong>Deaths</strong><br>Just as we don't have a great handle on the percent symptomatic, we don't completely
            know what proportion of those that have symptoms will have severe enough that they'll die. This
            proportion depends on the population characteristics of the community. Older persons with pre-existing health conditions
            or other risk factors will be at greater risk for serious illness and death. The
            WA state case data indicates that as of Mar 20, 2020, there were 83 deaths out of 1524 confirmed cases, or about 5%.  Some
            say the mortality rate is more like 1%.  We'll use the 1% estimate, which seems to be based on more data. Again,
            because confirmed cases are dependent on testing, which is limited, there are likely many more infections (infectious) persons 
            in the community than the 1524 persons. Below is a slider to provide an estimate for the % of infections that would result in
            deaths. We can see a plot of the number of illnesses and deaths during the outbreak. And below the figure is the model's 
            estimate of the cumulative number of people who become sick and who die from the outbreak. <br><br>"),
            HTML("Percent of Symptomatic that Die"),
            sliderInput("symptomatic_percent_death", "What percent of symptomatic persons die? (default:1)",
                        min = 1, max = 50, value = 1
            ),
        
            plotOutput("SIRplot_symptoms_deaths"),
            HTML("<br>"),
        
            HTML("<strong>Total Number of Illnesses by the end of 2 years:</strong>"),
            textOutput("numberSymptomatic"),
            HTML("<br>"),
            HTML("<strong>Total Number of Deaths by the end of 2 years:</strong>"),
            textOutput("numberDeaths"),
            HTML("<br>"),
            HTML("<strong>The deaths will probably lower than this</strong><br>
            Keep in mind, these are very crude estimates, without fully factoring in many uncertainties. Also keep in mind that
                 the total number of illnesses above, may range from very mild cases
                  that wouldn't require any health care -- to more severe cases that would require health care. In the
                 next section, we'll discuss 'serious symptomatics', and why the number of deaths may be 1/10th of the 
                 values found in this section.<br><br>")
            

        ),

        #Question 5###################################################
        tabPanel("5. What is this 'flattening the curve' I keep hearing about in the news?",
            h1("Flatten the Curve, Don't Overwhelm Healthcare Resources"),
            HTML("<br>"),
            img(src="fauci 3_10_2020.png", height=144, style="float:left; margin:10px"),
            HTML("<br><br><em>If you look at the curves of outbreaks, you know they go big peaks and then they come down, what we need to do is flatten that down.
                 -- Anthony Fauci, Director NIH NIAID, 3/10/2020</em><br style='clear:both'><br>"),
                 
            HTML("<strong>Flattening the Curve</strong><br>Notice that our previous SIR model simulations generally result in a classic epidemic curve
            over time, in which the number of infected (infectious) individuals grows exponentially because of the force of infection
            produced by infected on 
              susceptible people in the community. Eventually though, there won't be enough susceptibles left and the numbers of new infection 
              will no
            longer be as great as the number of infected people recovering. So the outbreak curve for the numbers of people in the 'I'
            group looks like a class outbreak 'peak'.<br><br>
            
            <strong>How big is the peak?</strong><br>
            The height of the peak matters because it illustrates at any given time what the requirements will be to service infected individuals.
            Remember that only a portion of these will be symptomatic, and only a portion of these symptomatic people will be so ill that they'll
            need heathcare services.  But, if we have limited Intensive Care Unit (ICU) beds, numbers of ventilators, or medical staff, 
            knowing the height of the peak and its duration will help us gauge the ability for healthcare to respond to the outbreak.
            According to <a href='https://www.usnews.com/news/best-states/washington/articles/2020-03-18/washington-state-scrambles-to-secure-hospital-beds-supplies'>
            this</a>, WA state has about 13,000 hospital beds.<br><br>
            
            <strong>Serious Symptomatic</strong><br>
            In the previous section, we finished by saying that symptomatics can range from mild to severe disease.
            With respect to hospital resource planning, it probably doesn't make sense to look at the mild cases.  So
            we'll introduce a new concept here of the 'Serious Symptomatic', which would be a severe case requiring
            hospitalization. This probably wouldn't be the 50% of infectious cases that we modeled in the previous step,
            but maybe more like 1 in 10 of those, or about 5% of infectious cases. From here on out when we
            refer to 'symptomatics' we'll be focusing on this 5% of severe cases for healthcare resource planning.<br><br>

            <strong>When will the peak occur?</strong><br>
            Also notice that the peak takes time to grow, and in the base case of 'if no intervention occurs' the epidemic will run its
            course relatively quickly because literally we did nothing to slow it down. With the default parameters, the peak number
            of infections crests at day 92 (Apr 2, 2020).<br><br>
            
            <strong>So is that the date when things will start to get better?</strong><br>
            Unfortunately, no.  Because that date was for the base case 'with no intervention'. Actually, as of Mar 20, 2020 there
            has been various interventions, for example school closures, recommendations that anyone
            with symptoms isolate themselves, closing down all but essential businesses, recommendations to 'hunker down' -- just shy
            of shelter-in-place orders that have already been mandated for other states in the US.
            <br><br>
            
            <strong>Interventions flatten the peak -- but they can also extend and delay it too</strong><br>
            We'll explore this below, by introducing an intervention that reduces the
            average period that people are infected/infectious. Think of this as
            what happens when sick people are isolated, like for example, when we hear 'stay at home
            if you're sick'.  For simplicity, we'll assume
            this intervention starts on day 1 and continues throughout the outbreak.<br><br>
            
            To implement this intervention via the model, below, move the slider for the 'Infectious Period' 
            to the left to lower its value. See what impact it has
            on the height, duration and timing of the peak. 
            <br><br>
            "),
            
            HTML("Effective Contact"),
            sliderInput("contact_rate_flatten", "How many person contacts with an infectious person would result in a new infection each day? (default:4)",
                        min = 1, max = 20, value = 4
            ),
            HTML("<em style='color:blue'>Infectious Period (try moving this slider to the left to isolate infectious people)</em>"),
            sliderInput("infectious_period_flatten", "After someone is infected, how may days are they infectious? (default:16)",
                        min = 1, max = 30, value = 15
            ),
            HTML("Population Size"),
            sliderInput("population_size_flatten", "How many people are in the community (in millions)? (default:4)",
                        min = 1, max = 50, value = 4
            ),
            HTML("Percent Severe Symptomatic"),
            sliderInput("symptomatic_percent_flatten", "What percent of infected (infectious) persons have severe symptoms? (default:5)",
                        min = 1, max = 75, value = 5
            ),
            
            HTML("Dashed red line indicates 13,000 hospital beds in the state, and the curves represent severe symptomatics."),            
            plotOutput("SIRplot_flatten"),
            HTML("<br>"),            
            
            htmlOutput("flatten_peak_height"),
            HTML("<br>"),
            HTML("The peak occurs at:"),
            textOutput("flatten_peak_time"),
            HTML("<br>We can see that implementing a strategy that isolates cases does flatten the peak, but it also extends the entire
                 outbreak, which can have dramatic secondary effects on the community (for example to schoolchildren's educations, 
                 to the economy, etc.  We also can observe that if we do nothing, we'll probably overwhelm our current
                 healthcare capacity. However, if we are aggressive in isolating cases, reducing the period that they
                 infect others by more than half we might be able to have health care capacity to deal with 
                 the surge in severe cases.)<br><br>")
        ),
  
        #Question 6###################################################
        tabPanel("6. Does isolating sick people help reduce transmission?",
             h1("Isolating Sick People"),
             HTML("<br>"),
             img(src="cuomo 3_9_2020.png", height=200, style="float:left; margin:10px"),
             HTML("<br><br><em>You want to find those people who are positive, investigate down that line, isolate as many positive people as you can, 
                  so you reduce the spread. Why do you want to reduce the spread? Because you want to protect the vulnerable population from this 
                  disease. -- Governor of New York, Andrew Cuomo, 3/9/2020</em><br style='clear:both'><br>"),
             
             HTML("Isolating sick individuals helps reduce transmission. In the previous modeling step in which we explored 'flattening the curve', we saw that by decreasing
             the time that people are infectious (for example, by isolating them at home, or isolated in healthcare
             settings if their symptoms are serious) by altering the value of the 'k' parameter in the model, we decreased the height of the peak number
             of infected and sick people, which can be important for not overwhelming healthcare resources.  It has the drawback of delaying and
             extending the peak of the outbreak though.<br><br>
             
             To recap the previous step, the static plot below compares the number of symptomatic people for the
             baseline 'do nothing' case (blue curve) vs an intervention that isolates cases by more than half
             the infectious period ('k' = 1/6)(red curve):<br><br>"),
             
             HTML("Dashed red line indicates 13,000 hospital beds in the state, and the curves represent severe symptomatics."),            
             img(src="static_flattened_curve.png", height = 427, width = 794),  # original: 1588 x 854
             HTML("<br><br>")

         ),
  
        #Question 7###################################################
        tabPanel("7. Aside from flattening the curve, don't we need to minimize total cases and deaths?",
           h1("Minimizing the Cumulative Numbers of Sick People and Deaths"),
           HTML("<br>"),
           img(src="cuomo 3_24_2020.png", height=144, style="float:left; margin:10px"),
           HTML("<br><em>My mother is not expendable. And your mother is not expendable. And our brothers and sisters are not expendable. 
           And we’re not going to accept a premise that human life is disposable. And we’re not going to put a dollar figure on human life.
           -- Governor of New York, Andrew Cuomo, 3/24/2020</em><br style='clear:both'><br>"),

           HTML("We observed in the previous two modeling steps that isolating cases flattens the curve, delaying the peak of the outbreak, 
           and the numbers of people with illness on any given day during the outbreak.  While this can be useful for not overwhelming our
           healthcare system, does flattening the curve actually reduce the total number of illnesses and deaths over the course of the 
           entire outbreak?  Let's re-run the model now, looking at total cumulative numbers of people who develop symptoms, and total numbers
           of deaths.  We'll run the model for three cases: a base scenario with 'no intervention', a scenario in which isolation of cases 
           results in 2/3rds the original infectious period, and a more aggressive scenario in which isolation of cases results in 1/2rd of the 
           original infectious period.<br><br>"),
           
           radioButtons(inputId="select_cumul_sick_death_scenario", label = "Select a scenario:", choices = 
                            c("Base scenario - no intervention (b=1/4, k=1/16) " = "base",
                              "Intervention - light (b=1/4, k=1/11)" = "light",
                              "Intervention - heavy (b=1/4, k=1/8)" = "heavy")),
           
           HTML("For each, we assume a community population of N=4 million people, 5% serious symptomatic percentage, and 1% death rate of those who are severely sick.<br><br>"),
           
           HTML("Below is the model of the cumulative numbers of symptomatic persons over time."),            
           plotOutput("SIRplot_cumul_sick_death_scenario"),
           HTML("<br><br>"),            
           
           HTML("For the scenario you selected:<br><br>"),            
           textOutput("number_cumul_sick"),
           textOutput("number_cumul_deaths"),
           HTML("<br>"),            
           HTML("The simulations suggest that with more aggressive isolation of symptomatic cases, not only do we flatten the curve,
           but we also reduce the cumulative numbers of sick people and deaths for the entire outbreak, which is certainly a good thing.<br><br>")

        ),
  
        #Question 8###################################################
        tabPanel("8. Does 'social distancing' reduce transmission?",
                 h1("Shut-downs, Working Remotely, Shelter-in-Place, and the Like"),
                 HTML("<br>"),
                 img(src="gavin_newsom 3_19_2020.png", height=230, style="float:left; margin:10px"),
                 HTML("<br><em>I have long believed that the future is not just something to experience. It’s something to manifest; that our fate and future is inside of us. It’s
decisions at the end of the day, not conditions, that determine that fate and future. We’re not victims of circumstance. We can make decisions
to meet moments, and this is a moment we need to make tough decisions. -- Governor of California, Gavin Newsom, 3/19/2020</em><br style='clear:both'><br>"),

                 HTML("<strong>Social Distancing</strong><br>
                 Isolating people with symptoms is a good first step to prevent the spread of infection. 
                 However, there are other more aggressive intervention steps based on the concept of 'social distancing'.
                 One next step might be to identify close-contacts of those with symptoms and quarantine these individuals because they are at risk of
                 having been infected and eventually developing symptoms or becoming asymptomatic spreaders of infection in the next few days. We wouldn't
                 want these obviously higher-risk individuals to further the spread infection -- so we need to keep them at a distance from the rest of society.
                 More aggressive steps might be aimed at reducing contacts <em>broadly</em> within the community. This is where closing down of schools, 
                 businesses, conventions, etc. come into play. And, eventually orders for the general population to shelter-in-place would be one of the most
                 aggressive interventions for social distancing.<br><br>
                 
                 <strong>Does social distancing work?</strong><br>
                 Yes, it does, by reducing the Effective Contact parameter 'b'.  Based on one of our previous modeling steps we fit WA case data to a 'b' of
                 1/4 or 'one out of every four contacts with an infectious person results in a newly infected person each day'. If we reduce overall contacts 
                 through social distancing interventions, we can think of this in essence as reducing this 'b' parameter.  Reducing contacts by
                 a factor of two would bring our 'b' to 1/8, and reducing it by a factor of three would bring it to 1/12.<br><br>
                      
                 Let's re-run the model now, looking at total cumulative numbers of people who develop severe symptoms, and total numbers
           of deaths.  We'll run the model for three cases: a base scenario with 'no intervention', a scenario in we've implemented a 'light' social distancing
           intervention, and a scenario with 'heavy' social distancing.<br><br>"),
                 
                 radioButtons(inputId="select_social_distance", label = "Select a scenario:", choices = 
                                  c("Base scenario - no intervention (b=1/4, k=1/16) " = "base",
                                    "Intervention - light (b=1/8, k=1/16)" = "light",
                                    "Intervention - heavy (b=1/12, k=1/16)" = "heavy")),
                 
                 HTML("For each, we assume a community population of N=4 million people, 5% severe symptomatic percentage, and 1% death rate of those who are severely sick.<br><br>"),

                 HTML("Below is the time-series of the epidemiological model SIR groups."),            
                 plotOutput("SIRplot_social_distance_SIR"),
                 
                 HTML("<br><br>"),            
                 HTML("Below is the model of the cumulative numbers of severe symptomatic persons over time."),            
                 plotOutput("SIRplot_social_distance"),
                 HTML("<br><br>"),            
                 
                 HTML("For the scenario you selected:<br><br>"),            
                 textOutput("number_social_distance_sick"),
                 textOutput("number_social_distance_deaths"),
                 HTML("<br>"),            
                 HTML("The simulations illustrate how social distancing reduces the cumulative numbers of severely sick persons
                      and deaths compared to the base case of 'doing nothing'. We also see that it delays
                      the outbreak peak, pushing it off in the future. With the more aggressive intervention,
                      we've pushed it almost off the 2-year figure.<br><br>And you know what they say about procrastination:<br>
                      <em>'Never put off till tomorrow... unless it's an outbreak.' -- Edmund.</em><br><br>")

        ),
  
        #Question 9###################################################
        tabPanel("9. Does the timing of interventions matter?",
           h1("Swift, decisive action to implement interventions"),
           HTML("<br>"),
           img(src="fauci 3_15_2020_3.png", height=144, style="float:left; margin:10px"),
           HTML("<br><em>Things will get worse before they get better. What we’re trying to do is to make sure they don’t get to the worst case scenario. 
                That’s what we need to do. -- Anthony Fauci NIH NIAID Director, 3/15/2020</em><br style='clear:both'><br>"),

           HTML("<strong>Start and Duration of Interventions</strong><br>
           We saw in previous modeling steps that interventions have the potential to alter the shape and timing of the 
           outbreak peak. However, in the previous models, we started implementing interventions on day 1. This wasn't the
           actual strategy employed in WA state. The efficacy of intervention strategies may depend on when they are implemented.
           Efficacy may also depend on the duration of interventions. We will explore this here 
           with a series of model scenarios. The first series will examine the impact of varying the start date of a hypothetical
           2-week shelter-in-place.  The second will examine the impact of varying the duration of the shelter-in-place
           order.<br><br>
           <strong>Varying the start of the intervention</strong><br>
           Below, we compare a baseline scenario of 'doing nothing' compared to a 2-week shelter-in-place scenario that
           starts on different dates that you'll select. The shelter-in-place reduces the effective contact 'b' to 1/12 for
           the 2-week period.  For other periods, the 'b' is the same as the baseline scenario, 'b' of 1/4.<br><br>"),
           
           radioButtons(inputId="select_timing_start", label = "Select a scenario:", choices = 
                            c("Base scenario - no intervention" = "base",
                              "Intervention - week 4 (Jan 22)" = "week4",
                              "Intervention - week 6 (Feb 5)" = "week6",
                              "Intervention - week 8 (Feb 19)" = "week8",
                              "Intervention - week 10 (Mar 4)" = "week10",
                              "Intervention - week 12 (Mar 18)" = "week12",
                              "Intervention - week 14 (Apr 1) {{ hint: this one's the best! }}" = "week14",
                              "Intervention - week 16 (Apr 15)" = "week16",
                              "Intervention - week 18 (Apr 29)" = "week18")),
           
           HTML("For each, we assume a community population with N=4 million people, k=1/16, 5% serious symptomatic percentage, and 1% death rate of those who are severely sick.<br><br>"),
           
           HTML("Below is the time-series of the epidemiological model SIR groups."),            
           plotOutput("SIRplot_timing_SIR"),
           HTML("<br>"),            
           
           HTML("For the scenario you selected:<br><br>"),            
           textOutput("number_timing_sick"),
           textOutput("number_timing_deaths"),
           HTML("<br>"),            
           
           HTML("<strong>But, hardly any changes happen!</strong><br>
                The simulation show that the timing matters slightly, but the effect is very minimal because
                2-weeks is perhaps too short of an intervention period. Let's now explore the effect of 
                altering the intervention duration.<br><br>"),            

           HTML("<strong>Varying the duration of the intervention</strong><br>
           Now, let's look at the effect of varying the duration of the intervention. We'll start the shelter-in-place intervention
           at week 12 (Mar 18, 2020), and you can select how many weeks it should last.<br><br>"),
           
           radioButtons(inputId="select_timing_duration", label = "Select a scenario:", choices = 
                            c("Intervention - lasts 1 week" = "week1",
                              "Intervention - lasts 2 weeks" = "week2",
                              "Intervention - lasts 3 weeks" = "week3",
                              "Intervention - lasts 4 weeks" = "week4",
                              "Intervention - lasts 5 weeks" = "week5",
                              "Intervention - lasts 6 weeks" = "week6")),
                                   
           HTML("Below is the time-series of the epidemiological model SIR groups."),            
           plotOutput("SIRplot_timing_SIR_duration"),
           HTML("<br>"),            
           
           HTML("For the scenario you selected:<br><br>"),            
           textOutput("number_timing_sick_duration"),
           textOutput("number_timing_deaths_duration"),
           HTML("<br>"),  
           
           HTML("The two series of simulations demonstrate that the timing and duration of interventions
           have an impact on reducing illnesses and deaths from the outbreak. We observed that interventions
           too soon or too late are not as effective as those timed 'just right'. This is important because
           we shouldn't assume that we can start interventions early and run them for long durations. People
           and our economy probably wouldn't put up with shelter-in-place for long periods of time!<br><br>
           
           However, when we looked at the effect of varying the duration of interventions, we did find that
           longer interventions are more effective in reducing illnesses and deaths than shorter interventions.
           It cuts down, and flattens/extends the peak a bit.  Notice how there's a blip of resurging 
                infection when we stop the intervention. We'll discuss this in the section 'When
                can we stop interventions, and return to our normal lives'.<br><br>")

        ),

        #Question 10###################################################
        tabPanel("10. I hear people talk about R0 for the virus -- what's that about?",
                 HTML("<h1>The Basic Reproduction Number, R<sub>0</sub></h1>"),
                 HTML("<br>"),
                 
                 HTML("<strong>Why does an epidemic occur in the first place?</strong><br>
           We saw in previous modeling steps that which infected persons interact with susceptible people, we get that classic outbreak
           peak that over time goes away as people gradually move into the 'R' group in the SIR model.  But why did the peak start in 
           the first place? This is explained by the 'Basic Reproduction Number' or R<sub>0</sub> -- the expected number of secondary cases
            produced by an infected person in a completely susceptible population.  If this number is exactly 1, then before this
            infected person moves to the 'R' category, they will have infected exactly one other person in the susceptible group. If R<sub>0</sub>
            is less than 0 then the outbreak would never have occurred at all because that infected person would not have spread his infection 
            to at least another person before they either recovered or died.  If however R<sub>0</sub> is greater than 1, then the numbers of
            infections will gradually increase. Mathematically, for the SIR model, R<sub>0</sub> is quantified by:<br><br>
            b/k = R<sub>0</sub><br><br>
            because if we look at the dI/dt equation we see that the terms with the rate parameters b and k balance each other out: if there are more 
            people moving into the 'I' category than are leaving to the 'R' category, then the number of infections will grow. Therefore, 
            b/k = R<sub>0</sub> > 1 is the condition for an epidemic to occur.<br><br>
            
            <strong>The R<sub>0</sub> for WA COVID-19 disease</strong><br>
            In our previous model fitting step, we found b and k to be approximately 1/4 and 1/16, respectively. So based on this crude
            model fitting, we'd estimate R<sub>0</sub> as:
            
            R<sub>0</sub> = b/k = (1/4)  / (1/16) = 4
            
            This is a bit on the high side compared to others' estimates of the COVID-19 R<sub>0</sub> for other contexts. But, it 
            indicates that in a completely susceptible population, one infected person would infect 4 others before moving to a 
            recovered state.<br><br>"),

            HTML("You can read more about R0 <a href='https://web.stanford.edu/~jhj1/teachingdocs/Jones-on-R0.pdf'>here</a>.<br><br>"),
            
            HTML("<strong>Effective Reproduction Number</strong><br>
            But the number of actively infected (infectious) didn't keep rising and rising. They peaked at some time, and then went down.
            Why did that happen if R<sub>0</sub> was greater than 1? Remember though that the basic reproduction number is really only relevant
            for describing what happens when a pathogen is introduced into a completely immuno-naive population -- like on day 1 of our modeling
            simulations.  After that, if infections spread, some people will likely become immune in the 'R' state. Then in this case, we 
            talk about another metric, the 'Effective Reproduction Number' or simply 'R' without the subscript 0 after it (note: this R is different from 
            the R in 'SIR')-- which is the
            average number of new infections per existing infected person when there are both susceptible and non-susceptible people in the 
            community -- like right after the disease starts to spread in the community. It is quantified by:<br><br>
            R = R<sub>0</sub> S(t)<br><br>
            where S(t) is the proportion of the population that's still susceptible at time t. If R<1 then the disease can no longer
            persist in the community, and the outbreak curve will fall off. Specifically, for our model in WA State, if we believe our
            estimate of R<sub>0</sub> = 4, then the threshold for COVID-19 persistence is when S(t) is 0.25 -- meaning the susceptible
            population needs to drop to below 25% of the total population of the community before new infections will decrease.<br><br>
            
            In reality though, once we start to implement intervention policies, the Effective Reproduction Number will change due to
            perturbations in 'b' and 'k' parameters.<br><br>

            You can read more about R <a href='https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/epidemic-theory'>here</a>.
                 <br><br>")

        ),

        #Question 11 ###################################################
        tabPanel("11. Why can't I get tested for COVID-19? Shouldn't everyone be tested?",
                 h1("The Importance of Early, Large-scale COVID-19 Testing"),
                 HTML("<br>"),
                 img(src="fauci 3_12_2020.png", height=144, style="float:left; margin:10px"),
                 HTML("<br><em>That is a failing. It is a failing. Let’s admit it. -- Anthony Fauci NIH NIAID Director, 3/12/2020</em><br style='clear:both'><br>"), 

                 HTML("<strong>Testing in the US</strong><br>
           It's not entirely clear what happened in Feb of 2020 when cases of COVID-19 began to surge in WA state. 
           Future investigations will hopefully explain the <em>numerous</em> issues with the faulty federal CDC test kits and their 
           lack of availability. 
           In the absence of reliable federal testing kits within WA state, the University of Washington implemented testing. However,
           their testing was initially limited to certain conditions, such as having symptoms, a history of contact with COVID-19 cases, etc.
           -- rather than a massive public testing program, which seems to have been implemented in other countries.<br><br>
           
           Early testing can play an important role in isolating cases. In our previous modeling steps ('What is this flattening of the curve I keep
           hearing about in the news?' and 'Does isolating sick people help reduce transmission?'), we demonstrated the effect of
           isolating cases from day 1 of an outbreak.  It effectively flattens the epidemic curve.  And, in the step ('Aside from flattening
           the curve, don't we need to minimize total cases and deaths?') we saw that the total overall illnesses and deaths could
           have been reduced if more testing and aggressive identification and isolation of cases and quarantining of close contacts was 
           carried out.<br><br>
           
           As of Mar 20, 2020 there is still no testing available for the general public. This is a major public health failure because
           we have no way of identifying asymptomatic individuals (people who are infected and infectious, yet do not display any symptoms).
           These individuals may still be spreading infection in the community, especially since there is no mandated and enforced 
           shelter-in-place in the state. It remains unclear if people are complying with recommendations to 'hunker down' or are capable
           of even doing so.<br><br>
           
           The lack of testing in high-risk populations is especially worrisome. I'm not only talking about testing those at 
           high-risk of illness and death, such as seniors, people with pre-existing health conditions, pregnant women, etc.  I'm 
           also talking about those at high-risk <em>of spreading infection</em>. These might be people who care for seniors,
           healthcare professionals, or people who contact many people in the community (for example, operators of
           'essential services' such as public transit operators, and restaurant, grocery, pharmacy service workers, etc.).<br><br>
           
           Some might argue that it's not possible to test everyone. But, why can't it be done given the potential reductions to illnesses
           and deaths?<br><br>")
                 
        ),
  
        #Question 12 ###################################################
        tabPanel("12. Why is a seroprevalence test even more important than a COVID-19 diagnostic test right now?",
           h1("Information on How Many People in the Community Have Been Exposed is Critical to Planning Interventions"),
           HTML("<br>"),
           img(src="fauci 3_15_2020.png", height=200, style="float:left; margin:10px"),
           HTML("<br><br><em>I hope we don’t see the second blip, but it’s possible. — Anthony Fauci NIH NIAID Director, 3/15/2020</em><br style='clear:both'><br>"),
           
           HTML("<strong>Seroprevalence data will inform how long we need to maintain our interventions</strong><br>
           Diagnostic tests to determine if a person is currently infected with COVID-19 exist, but aren't generally available, which is 
           a problem discussed in the previous section ('Why can't I get tested for COVID-19? Shouldn't everyone be tested?').  A seroprevalence
           test however, is different in that it tests whether a person has been exposed and previously infected with COVID-19. These tests
           usually look for specific antibodies that the body's immune system produces to combat COVID-19. People who'd test positive would 
           likely be individuals that are in the 'R' recovered state.<br><br>
           
           <strong>Relationship to the Effective Reproduction Number</strong><br>
           In the previous section ('I hear people talk about R0 for the virus -- what's that about?'), the Effective Reproduction Number
           'R' was described as the condition that must be met in order for disease transmission to persist in the community.  We want this 
           'R' to be lower than 1 so that infections will decay with time. To recap, the relationship was:
           
           R = R<sub>0</sub> S(t)<br><br>
           where S(t) is the proportion of the population that's still susceptible at time t. If R<1 then the disease can no longer
            persist in the community, and the outbreak curve will fall off (it might take awhile for it to fall to zero though).
            And, we found that with an estimated R<sub>0</sub> = 4, 
            S(t) needed to be below 0.25 -- in other words, the susceptible population needs to drop to below 25% of the 
            total population of the community before infections will start to decrease.<br><br>
            
            How do we actually know if we've reached S(t) < 25%?  That's where a seroprevalence test would be helpful. If we test a 
            representative sample of the WA state population and find that more than 75% are seropositive, we can safely
            reduce our interventions, people can go back to work, kids back to school, let businesses move forward, etc. -- all while the 
            disease would naturally go away.<br><br>
            
            If however, we lift interventions too soon, there may be a risk of resurgence in infections because there are still
            too many susceptible individuals in the community, and it's hard to know whether there are any symptomatic or asymptomatic
            infected remaining in the community who could restart disease transmission.<br><br>
            
            We can explore that here with the following simulation scenarios (the intervention is a social distancing strategy 
            that reduces 'b' from 1/4 to 1/8 starting in week 11, and lasting for the 
                specified duration):<br><br>"),
            
           radioButtons(inputId="select_seroprevalence", label = "Select a scenario:", choices = 
                c("Intervention lasting 3 weeks (too short)" = "short",
                  "Intervention lasting 6 weeks (too short)" = "longer",
                  "Intervention lasting 14 weeks (long enough)" = "long enough")),
            
           HTML("Below is the time-series of the epidemiological model SIR groups."),            
           plotOutput("SIRplot_seroprevalence"),
           HTML("<br>"),            
           
           HTML("The model results demonstrate that if we stop an intervention before S(t) reaches below 0.25 (25% of the total population)
                there is a blip of re-infections that occurs once the intervention is stopped. For the last scenario, in which we
                allowed the intervention to run to 14 weeks, when S(t) ~ 0.25, we no longer get a blip of re-infection, and the infection
                curve continues to go to zero. So it would be very useful to have a seroprevalence test.<br><br>")            
        ),
 
        #Question 13 ###################################################
        tabPanel("13. When can we stop interventions, and return to our normal lives?",
           h1("Be Careful About Terminating Interventions Too Early"),
           HTML("<br>"),
           img(src="fauci 3_24_2020.png", height=200, style="float:left; margin:10px"),
           HTML("<br><br><em>On the subject of stopping interventions: 'People might get the misinterpretation you’re going to just lift everything up, and even somebody’s going like 
           ‘that’ [gestures up]. That’s just not going to happen. 
                It’s going to be looking at the data.' -- Anthony Fauci NIH NIAID Director, 3/24/2020</em><br style='clear:both'><br>"),

           HTML("<strong>There will be a resurgence of infections if interventions are terminated too soon</strong><br>
           In the previous section ('Why is a seroprevalence test even more important than a COVID-19 diagnostic test right now?') I
           demonstrated how seroprevalence and the Effective Reproduction Number need to be considered before stopping interventions.
           The model simulations in that section showed that if we stop interventions before the the proportion of the susceptibles
            in the population is 1/R<sub>0</sub> or about 0.25, then there will be a resurgence of infection when we stop the interventions.
            This is because there are still a sufficiently large enough population of susceptibles that can become infected
            and it is hard to know whether there are no more infected (infectious) people in the community that can re-start the spread
            of infection.<br><br>
           
           Oh, and another thing: if S(t) is still high, and an infected person travels from another place into WA state, they'll kick-start
           a re-emergence of infection too!<br><br>")
        ),
        

        #Question 14 ###################################################
        tabPanel("14. Should we shut down schools? What about shutting down [fill in blank]?",
            h1("Limiting High Contact Sites"),
            HTML("<br>"),  
            img(src="hayes 3_6_2020.png", height=144, style="float:left; margin:10px"),
            HTML("<br><br><em>It is not warranted to close a school unless there has been a COVID-19 case in that school...
            Children... we do not know how they play into the transmissibility of COVID-19.
                 -- Patty Hayes, Director Public Health Seattle & King County, 3/6/2020</em><br style='clear:both'><br>"),

            HTML("<strong>Heterogeneous Populations</strong><br>
           In the previous modeling sections, we assumed a very basic SIR model for the 3-county region of WA state. This is a very crude
           over-simplification, which assumes <em>average</em> contact rates and <em>average</em> recovery rates for the population.
            In the real-world there are differences between people and population groups. Across the state, the probability of person-to-person
            contacts might be greater for a higher density region like King County than some of the state's rural counties. Also,
            there are sites such as schools, workplaces, churches, public transit, etc. where many people might come in close contact with
            each other. While social distancing generally can be effective in reducing contacts, it probably makes sense to focus 
            on limiting contacts at those places/situations where they're more likely to occur. Many schools closed
            during the week of Mar 9th (week 11 of the simulation).<br><br>
            
            To explore this further, we'll run a special two-group SIR model, where we'll have one group 'A' of people in one SIR model
            and another group 'B' in another SIR model.  Group A will have higher effective contacts than group B.  We can think of Group
            A as those people who have lots of contacts, like at a school.  But, the infections in
            either group A and B can contribute to new infections in either group.  Let's see what happens for various scenarios (baseline of no
                 intervention vs different social distancing scenarios for Group A lowering b from 1/3 to 1/12 starting at
                 week 11 and lasting for the specified duration, while Group B remains at b of 1/8, with 70% of the population in group A and 30% in group B):<br><br>"),
           
            radioButtons(inputId="select_heterogeneous", label = "Select a scenario:", choices = 
                                 c("No Intervention (base case)" = "base", 
                                   "Intervention in Group A (4 weeks)" = "interv1",
                                   "Intervention in Group A (8 weeks)" = "interv2",
                                   "Intervention in Group A (16 weeks)" = "interv3")),

            HTML("Below is the time-series of proportion of community population that is infected in groups A and B for both coupled SIR models."),
            plotOutput("SIRplot_heterogeneous"),
            HTML("<br>"), 
            
            HTML("For the scenario you selected:<br><br>"),            
            textOutput("number_heterogeneous_sick"),
            textOutput("number_heterogeneous_deaths"),
            HTML("<br>"),  
            
            HTML("The simulations show that intervention in the high-contact group A
            not only reduces the infection peak for group A, but it also lowers the infection peak for group B. This
            is because infections in group A also contribute to new infections among group B in the coupled model.
            This illustrates how social distancing implemented for certain groups can have secondary beneficial impacts for other
            segments of the population. If we consider that group B individuals might be our high-risk seniors in the community,
            we can understand how social distancing strategies aimed at younger group A people not only benefit the young, but 
                 seniors too.<br><br>")

        ),

        #Question 15 ###################################################
        tabPanel("15. Should we limit travel?",
            h1("Imported Disease"),
            HTML("<br>"),  
            img(src="fauci 3_15_2020_2.png", height = 144, style="float:left; margin:10px"),
            HTML("<br><br><em>The president’s decision to essentially have a major blocking of travel from China, that  already had an effect of not seeding…
— Anthony Fauci NIH NIAID Director, 3/15/2020</em><br style='clear:both'><br>"),
                 
            HTML("<strong>Travel and importation of disease can kick-start new infections</strong><br>
           In the previous modeling section ('Should we shut down schools? What about shutting down [fill in blank]?'), we started to 
           explore coupled SIR population models. This sampled coupled model may be used to examine the effect of limiting travel (or not) between
           two communities.<br><br>
           
           Let's examine what happens if we do or don't limit travel between two communities A and B. To 
                 make it easier to see the effect, let's say that under the baseline scenario, community B's 
                 outbreak would have normally occur 1 month after community A's outbreak. The travel introducing a mixing parameter
                 between the two communities' 'b' parameters:<br><br>"),
           
            radioButtons(inputId="select_travel", label = "Select a scenario:", choices = 
                             c("Two isolated communities (base case)" = "base", 
                               "Allowing a little travel between communities A and B (0.02 mixing)" = "little",
                               "Allowing more travel between communities A and B (0.05 mixing)" = "more")),
            
            HTML("Below is the time-series of proportion of community population that is infected in communities A and B for both coupled SIR models."),
            plotOutput("SIRplot_travel"),
            HTML("<br>"), 
            
            HTML("For the scenario you selected:<br><br>"),            
            textOutput("number_travel_sick"),
            textOutput("number_travel_deaths"),
            HTML("<br>"),  
            
            HTML("The simulations show that limiting travel keeps the timing of the outbreaks separated between the two communities.
            However, allowing  travel between the communities, shifts the timing of the outbreaks so that they start to match
            each other.  Basically as soon as infection enters from community B from community A through say air travel, infection
                 will 'take-off' in community B because its R<sub>0</sub> is like A's, suitable for transmission.<br><br>")
        ),
  
        #Question 16 ###################################################
        tabPanel("16. If we institute a 'shelter-in-place' policy in one city, but not other cities, will it still be effective?",
            h1("Disease Persistence Across Metapopulations"),
            HTML("<br>"),  
            img(src="cuomo 3_17_2020.png", height=144, style="float:left; margin:10px"),
            HTML("<br><br><em>You close the bars in New York City, but you keep them open in Nassau, 
                 all you’d see is a flood of cars going to bars in Nassau. — Governor of New York, Andrew Cuomo, 3/17/2020</em><br style='clear:both'><br>"),
                 
            HTML("<strong>Travel makes it difficult to sustain the efficacy of interventions</strong><br>
           In the previous section ('Should we limit travel?') I showed how disease can be imported between communities, kick-starting
           new outbreaks. There's another phenomenon that we've learned about coupled populations from the Ecological Sciences,
           which is related to 
           disease persistence. Different population groupings are called a 'metapolulation'. For our purposes, let's consider 
           a group of different cities as our metapopulation. Let's make it simple and say there are just two cities in our 
           metapopulation, one is A and the other is B.<br><br>
           
           To make this provocative, let's say that one city A is doing its best to implement social distancing as an 
            intervention, while the other city B isn't doing anything to limit the spread of infection. We will consider a
                 baseline scenario, in which we do not allow for any travel between the two cities, so 
                 their outbreaks should be decoupled and progress as they would on their own.  However, we'll allow
                 for alternative scenarios with either a little, or slightly more travel between the communities. City A's 'b' is 1/5 because
                 they implemented social distancing, whereas city B's 'b' is 1/4. Travel is implemented as a mixing variable on 
                 the 'b' parameter.  Let's see what happens:<br><br>"),
                 
            radioButtons(inputId="select_metapop", label = "Select a scenario:", choices = 
                             c("Two isolated cities (base case)" = "base", 
                               "Allowing a little travel between cities A and B (0.02 mixing)" = "little",
                               "Allowing more travel between cities A and B (0.05 mixing)" = "more")),
            
            HTML("Below is the time-series of proportion of community population that is in the 'I' infected state in cities A and B for both coupled SIR models."),
            plotOutput("SIRplot_metapop"),
            HTML("<br>"), 
            
            HTML("For the scenario you selected:<br><br>"),            
            textOutput("number_metapop_sick"),
            textOutput("number_metapop_deaths"),
            HTML("<br>"),  
            
            HTML("We can observe from these simulations that even though city A is doing the right thing by implementing a
                 social distancing intervention, travel between a city B that isn't doing an intervention ends up hurting
                 city A. The mixing of the populations via travel not only pushes the infection peak earlier for A than 
                 it would otherwise have occurred if it limited travel, but it also increases the illnesses and deaths
                 in city A.<br><br>")
        ),

        #Question 17 ###################################################
        tabPanel("17. What about an SEIR model that considers exposure latency?",
        h1("Fitting an SEIR Model"),
        HTML("<br>"),  
        img(src="Neil Ferguson 2_15_2020.png", height=144, style="float:left; margin:10px"),
        HTML("<br><br><em>On uncertainty of model estimates and usefulness of results:<br> 
             ‘It informs what in the UK is called the reasonable worst case scenario. 
             It informs planning, the kind of scale of planning countries should be prepared to undertake’
              -- Neil Ferguson, Professor Imperial College London, 2/15/2020</em><br style='clear:both'><br>"),
        
        HTML("In the Introduction section 'How do models work?', I described an alternative
        to the SIR model, which is the SEIR model that accounts for latency between exposure to the virus
        and becoming infectious. Because the SEIR model has an additional population state, it requires
        additional parameters to be specified in order for it be implemented. Using the same WA case data
        provided in the previous section 'What does the situation look like in WA state?', we'll attempt
        to fit the parameters for the SEIR model, and run it for a few scenarios.<br><br>
        
        <strong>SEIR parameter estimation</strong><br>
        Below are sliders that can be changed to run the SEIR model with different parameter values. The effective contact
        is 1/beta; the latency period is 1/alpha; the infectious period is 1/gamma; and we'll assume the birth and non-COVID-19 death
        rate mu is 0 to keep things simple.  As you change the parameters to run the model you can see a plot of its
        fit for the WA state data, a goodness of fit RMSE estimate, and a separate plot of the time-series of the population 
        states so that you can see what the outbreak curve looks like.
        <br><br>"),
        
        HTML("Effective Contact"),
        sliderInput("SEIR_contact_rate_fit", "How many person contacts with an infectious person would result in a new infection each day? (default:2)",
                    min = 1, max = 20, value = 2
        ),

        HTML("Latency Period"),
        sliderInput("SEIR_latency_rate_fit", "How many days is the latency period? (default:6)",
                    min = 1, max = 20, value = 6
        ),
        
        HTML("Infectious Period"),
        sliderInput("SEIR_infectious_period_fit", "After someone is infected, how may days are they infectious? (default:22)",
                    min = 1, max = 30, value = 22
        ),
        
        HTML("<strong>Goodness of Fit</strong><br>The RMSE is an indication of the how well the model fits the data:"),
        textOutput("SEIR_fit_RMSE"),
        HTML("<br>Below, the bars are the WA state case data, and the blue line represents the model fit."),
        
        plotOutput("SEIRplot_fit"),
        plotOutput("SEIRplot"),
        
        HTML("<strong>SEIR parameter uncertainty</strong><br>
          If we run many combinations of the 3 parameters, we'll find that different combinations give similar
          low RMSE fit values.  For example:<br><br>
          1/beta = 3; 1/alpha = 2; 1/gamma = 20; RMSE=  0.0209<br>
          1/beta = 2; 1/alpha = 6; 1/gamma = 22;RMSE =  0.0213<br><br>
          which have considerably lower RMSE values than other parameter combinations. It's hard to know which of these
          two parameter combinations is 'practically' better
          than the other, and so let's explore the impact of this uncertainty by looking at the difference in both 
          times-series plots:"),
        
        img(src="SEIR_two_versions.png"),
        
        HTML("<br>We can see that the major differences are in peaks for the 'E' exposed groups. The second set of parameter values
        has a higher E peak that's roughly twice as high as the other.  But the 'I' infectious curves look very similar.
        And that's probably why they're both considered good fits to the WA state case data. But what if we implement
        interventions using these two parameter sets?<br><br>"),
        
        HTML("<strong>Social distancing intervention with SEIR model</strong><br>
          Below we have implemented a social distancing intervention SEIR model, that allows for us to specify the
          start of the intervention, how long it will last, and the magnitude of the intervention.<br><br>
          "),
        
        HTML("Start of intervention"),
        sliderInput("SEIR_intervetion_start", "Which week (week 1 is Jan 1, 2020)? (default:11)",
                    min = 1, max = 20, value = 11
        ),
        
        HTML("Duration of intervention"),
        sliderInput("SEIR_intervention_duration", "How many weeks does the intervention last? (default:4)",
                    min = 1, max = 30, value = 4
        ),
        
        HTML("Magnitude of intervention"),
        sliderInput("SEIR_intervention_magnitude", "Divide the amount of social contacts by: (default:4)",
                    min = 1, max = 100, value = 4
        ),
        
        selectInput(inputId = "SEIR_intervention_whichModel", label = "Select which parameter set to show:",
                    choices = c("First set of parameters", 
                                "Second set of parameters", 
                                "Both"), selected = "Both"),
        
        plotOutput("SEIRplot_intervention"),
        
        HTML("<strong>Intervention effects on the Peak</strong><br>
        We can observe from changing the intervention settings that similar phenomena occur with the SEIR model as we
        have previously observed with the SIR model. Interventions can cut down on the infection peak, but also delay it
        as well. Delaying the peak, and stopping interventions when there are still a large number of susceptibles in 
        the community will result in a resurgence of infection when the intervention is stopped.<br><br>"),

        HTML("<strong>R<sub>0</sub> for the SEIR model</strong><br>
        In the absence of interventions, R<sub>0</sub> for the SEIR model is<br><br>
        R<sub>0</sub> = (beta*alpha)/((mu+alpha)*(mu+gamma))<br><br>
        For the two parameter sets we fit, we get a range of R0 of 6.7 to 11, which seems unrealistically high.
        More realistic values are reported <a href='http://gabgoh.github.io/COVID/index.html'>here</a> 
        from fitting other data sets from around the world, which range from 1.4 to 7.23 (the 14.8 is for the cruise
        ship).<br><br>
        Let's stop here, but more work may be needed to fit the SIR and SEIR models better to Washington state
        data.<br><br>")

         ),
  
      #Summary###############################################################
      "Summary",
      ########################################################################
  

        #Question 18 ###################################################
         tabPanel("What does this all mean? How can I help?",
         ########################################################################
         h1("Lessons learned, and lessons still to learn"),
          HTML("<br>"),  
          img(src="inslee 3_20_2020.png", height=144, style="float:left; margin:10px"),
          HTML("<br><br><em>We understand that perhaps the force of law will not be necessary, 
          IF — IF Washingtonians act with the force of compassion, in the force of responsibility, 
          in force of a sense that we are indeed all in this together.  — Governor Jay Inslee, 3/20/2020</em><br style='clear:both'><br>"),
         
         HTML("There are some important lessons that I think these modeling exercises provide to help
         explain some of the messaging we're hearing from public health authorities. When we hear 
         things on the news, to practice social distancing, limit
         travel, and staying at home if we're sick -- it may not be entire clear why they're asking us 
         to do these things. Yet, models help explain the ramifications of our
         actions or inactions in these regards.<br><br>
         
         As of Mar 24, 2020, the public is starting to face question 13 ('When can we stop interventions, and return to our normal lives?').
         Because we pushed the outbreak peak out longer with our interventions, and we don't have a clear understanding
         of the seroprevalence in the community, or how much risk there may be from re-importation of disease if there are 
         still many susceptible people in the state, making the decision to stop interventions will be just as difficult 
         as deciding to start them.<br><br>
         
         Keep in mind that all models have limitations. Therefore, the answers provided here should not
         be considered accurate or precise given the limitations, assumptions, and variability and uncertainties
         in parameter estimates. Therefore, you should not rely solely on a single model and set of results for decision-making.
         There are alternative epidemiological models that may be considered. For example, this is site with an
  <a href='http://gabgoh.github.io/COVID/index.html'>SEIR model</a>.<br><br>

         <strong>If you'd like to help</strong><br>
         Please spread the word to help your friends and family understand the rationale for and importance of outbreak interventions.<br><br>
         
         If you'd like to explore the R code for these models yourself, I'll post them on my website <a href='http://www.edmundseto.com'>
         http://www.edmundseto.com</a> when I have a chance.<br><br>
         
         If you'd like to support my students who work on societal problems such as this, please consider a donation to my research group:
         <a href='https://sph.washington.edu/giving'>https://sph.washington.edu/giving</a>, mentioning 'Gift to Dr. Edmund Seto, Professor of 
         Environmental & Occupational Health Sciences'. We have many projects such as this that could use your support.
         <br><br>")

        )

        
        #Summary###############################################################
        #"Summary",
        ########################################################################
        #tabPanel("Configure",
        #    shinythemes::themeSelector()
        #)
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    ### Question: case data from WA ####
    ########################################
    output$WA_casesPlot <- renderPlot({
        ggplot(WA_cases, aes(x = as.Date(date, "%m/%d/%Y"), y=confirmed_cases)) + 
            geom_bar(stat = "identity") +
            xlab("Date") + ylab("Confirmed Cases")
    })

    output$WA_deathsPlot <- renderPlot({
        ggplot(WA_cases, aes(x = as.Date(date, "%m/%d/%Y"), y=deaths)) + 
            geom_bar(stat = "identity") +
            xlab("Date") + ylab("Deaths")
    })
    
    output$WA_casesData <- renderDataTable({ datatable(WA_cases, options = list(pageLength = 5)) })
    
    
    ### Question: model fit ####
    ########################################
    
    calcModel_fit <- reactive({
        ### SIR model
        parameters <- c(b = 1/(input$contact_rate_fit),
                        k = 1/(input$infectious_period_fit))
        
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -b*S*I 
                dI <- b*S*I - k*I
                dR <- k*I
                
                # return the rate of change
                list(c(dS, dI, dR))
            }) # end with(as.list ...
        }
        
        times <- seq(0, 80, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters)
        data.frame(out)   # returns wide dataframe
    })    
    
    output$SIRplot_fit <- renderPlot({
        WA_cases$day <- seq(21,21+nrow(WA_cases)-1)
        df <- calcModel_fit()
        df$infections_prop_ofEnd <- (1-df$S)/(1-df[nrow(df),"S"])
        
        ggplot() + 
            geom_bar(data=WA_cases, aes(x = day, y=confirmed_cases/WA_cases[nrow(WA_cases),"confirmed_cases"]), stat = "identity") +
            xlab("Day") + ylab("Proportion of confirmed cases, up to Mar 20, 2020") +
            geom_line(data=df, aes(x=time, y=infections_prop_ofEnd), col="blue")
           # geom_line(data=df, aes(x=time, y=S))
    })
    
    calc_FitRMSE <- reactive({
        WA_cases$day <- seq(21,21+nrow(WA_cases)-1)
        WA_cases$infections_prop_ofEndWA <- ((WA_cases$confirmed_cases) / (WA_cases[nrow(WA_cases),"confirmed_cases"]))
        df <- calcModel_fit()
        df$infections_prop_ofEndModel <- (1-df$S)/(1-df[nrow(df),"S"])
        fitdf <- merge (df, WA_cases, by.x="time", by.y="day", all.x=FALSE, all.y=TRUE)
        fitdf$diff <- fitdf$infections_prop_ofEndModel - fitdf$infections_prop_ofEndWA
        fitdf$diffsq <- fitdf$diff^2
        fitdf  # this is returned as dataframe
    })
    
    output$fit_RMSE_dt <- renderDataTable({ datatable(calc_FitRMSE(), options = list(pageLength = 5)) })

    output$fit_RMSE <- renderText({
        sqrt(mean(calc_FitRMSE()$diffsq))
    })   
    
    ### Question: How many infections ####
    ########################################
    
    calcModel <- reactive({
        ### SIR model
        parameters <- c(b = 1/(input$contact_rate),
                        k = 1/(input$infectious_period))
        
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -b*S*I 
                dI <- b*S*I - k*I
                dR <- k*I
                
                # return the rate of change
                list(c(dS, dI, dR))
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters)
        data.frame(out)   # returns wide dataframe
    })
    
    output$SIRplot <- renderPlot({
        df <- melt(calcModel(), id=c("time"))   # make long
        ggplot(df, aes(x=time, y=value, group=variable)) + geom_line(aes(col=variable)) +
            xlab("Day") + ylab("Proportion of the population")
    })
    
    
    output$numberInfected <- renderText({
        prop_infected <- 1-(calcModel()[nrow(calcModel()),"S"])
        paste("For your model parameters above, after 2 years, an estimated ", signif(prop_infected*100, 3), "% of the population, or approx. ", 
              signif(prop_infected*(input$population_size),3), "million persons will have been infected.")
    })
        
    ### Question: Symptomatic cases and deaths ####
    ########################################
    
    calcModel_symptoms <- reactive({
        ### SIR model
        parameters <- c(b = 1/(input$contact_rate_symptoms),
                        k = 1/(input$infectious_period_symptoms))
        
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -b*S*I 
                dI <- b*S*I - k*I
                dR <- k*I
                
                # return the rate of change
                list(c(dS, dI, dR))
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters)
        data.frame(out)   # returns wide dataframe
    })
    
    output$SIRplot_symptoms <- renderPlot({
        df <- calcModel_symptoms()
        df$symptomatic <- df$I * input$symptomatic_percent/100
        df <- melt(df, id=c("time"))   # make long
        ggplot(df, aes(x=time, y=value*input$population_size_symptoms, group=variable)) + geom_line(aes(col=variable)) +
            xlab("Day") + ylab("# of persons (millions)")
    })
    
    ## (1-S)*(symptomatic%) at end time of simulation is the proportion of population that were symptomatic.
    ## I*(symptomatic%) is the proportion of the population that is symptomatic at any point in time.  
    
    output$SIRplot_symptoms_deaths <- renderPlot({
        df <- calcModel_symptoms()
        df$symptomatic <- df$I * input$symptomatic_percent/100
        df$deaths <- df$symptomatic * input$symptomatic_percent_death/100
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "symptomatic" | df$variable == "deaths",], aes(x=time, y=value*input$population_size_symptoms, group=variable)) + geom_line(aes(col=variable)) +
            xlab("Day") + ylab("# of persons (millions)")
        
    })
    
    output$numberSymptomatic <- renderText({ 
        df <- calcModel_symptoms()
        symptomatic <- (1-df[nrow(df),"S"]) * input$symptomatic_percent/100
        paste(format(round(symptomatic * input$population_size_symptoms * 1e6, 0), big.mark=","), "persons")  # this is returned
    })

    output$numberDeaths <- renderText({ 
        df <- calcModel_symptoms()
        symptomatic <- (1-df[nrow(df),"S"]) * input$symptomatic_percent/100
        paste( format(round(symptomatic * input$symptomatic_percent_death/100  * input$population_size_symptoms * 1e6, 0), big.mark=","), "persons") # this is returned
    })

    ### Question: Flatten the peak ####
    ########################################
    
    # this is a slight different model since it runs two curves, original, and shifted.
    calcModel_flatten <- reactive({
        ### SIR model
        parameters <- c(b = 1/(input$contact_rate_flatten),
                        k = 1/(input$infectious_period_flatten))
        
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0,
                   S_orig = 1,
                   I_orig = 1e-7,
                   R_orig = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -b*S*I 
                dI <- b*S*I - k*I
                dR <- k*I
                dS_orig <- -b*S_orig*I_orig 
                dI_orig <- b*S_orig*I_orig - (1/16)*I_orig
                dR_orig <- (1/16)*I_orig
                
                # return the rate of change
                list(c(dS, dI, dR, dS_orig, dI_orig, dR_orig))
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters)
        data.frame(out)   # returns wide dataframe
    })


    output$SIRplot_flatten <- renderPlot({
        df <- calcModel_flatten()
        df$symptomatic <- df$I * input$symptomatic_percent_flatten/100
        df$symptomatic_original <- df$I_orig * input$symptomatic_percent_flatten/100

        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "symptomatic" | df$variable == "symptomatic_original",], aes(x=time, y=value*input$population_size_flatten*1e6, group=variable)) + 
            geom_line(aes(col=variable)) +
            geom_hline(yintercept=13000, col="red", linetype = 2) +
            xlab("Day") + ylab("# of persons")
    })
    
    output$flatten_peak_height <- renderText({
        df <- calcModel_flatten()
        peak_symptomatic <- max(df$I) * input$population_size_flatten * 1e6 * (input$symptomatic_percent_flatten/100)
        paste("At the height of the peak, the number of symptomatics:", format(round(peak_symptomatic, 0), big.mark=","), 
              "<br>At the height of the peak, the number of deaths (assuming", 
              format(round(default_deaths*100,0)),
              "% of symptomatics):", format(round(peak_symptomatic*default_deaths, 0), big.mark=","))  # this is returned   
    })

    output$flatten_peak_time <- renderText({
        df <- calcModel_flatten()
        peak_day <- df[which.max(df$I),"time"]
        paste("Day:", round(peak_day, 0), " (", as.Date("2020-01-01") + peak_day, ")")  # this is returned   
    })
    
    
    ### Question: Cumulative sick and deaths ####
    ########################################
    
    # this is a slight different model since it runs two curves, original, and shifted.
    calcModel_cumul_sick_death <- reactive({
        ## which scenario?
        if (input$select_cumul_sick_death_scenario == "base") {
            parameters <- c(b = 1/(4),
                            k = 1/(16))
        } else if (input$select_cumul_sick_death_scenario == "light") {
            parameters <- c(b = 1/(4),
                            k = 1/(11))
        } else { # heavy
            parameters <- c(b = 1/(4),
                            k = 1/(8))
        }

        ### SIR model
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0,
                   S_orig = 1,
                   I_orig = 1e-7,
                   R_orig = 0)

        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -b*S*I 
                dI <- b*S*I - k*I
                dR <- k*I
                dS_orig <- -b*S_orig*I_orig 
                dI_orig <- b*S_orig*I_orig - (1/16)*I_orig
                dR_orig <- (1/16)*I_orig
                
                # return the rate of change
                list(c(dS, dI, dR, dS_orig, dI_orig, dR_orig))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters)
        data.frame(out)   # returns wide dataframe
    })
    
    output$SIRplot_cumul_sick_death_scenario <- renderPlot ({
        df <- calcModel_cumul_sick_death()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        df$cumulative_symptomatic_base_case <- (1-df$S_orig) * default_severe_symptomatic
        
        df <- melt(df, id=c("time"))   # make long
        if (input$select_cumul_sick_death_scenario != "base") {
            ggplot(df[df$variable == "cumulative_symptomatic" | df$variable == "cumulative_symptomatic_base_case",], aes(x=time, y=value*default_popn*1e6, group=variable)) + 
                geom_line(aes(col=variable)) +
                xlab("Day") + ylab("Cumulative # of persons over time")
        } else {
            ggplot(df[df$variable == "cumulative_symptomatic",], aes(x=time, y=value*default_popn*1e6, group=variable)) + 
                geom_line(aes(col=variable)) +
                xlab("Day") + ylab("Cumulative # of persons over time")
        }
    })
    
    output$number_cumul_sick <- renderText ({
        df <- calcModel_cumul_sick_death()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of symptomatic persons by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"]* default_popn*1e6,0), big.mark=","), "persons.")
    })
    
    output$number_cumul_deaths <- renderText ({
        df <- calcModel_cumul_sick_death()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of deaths by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.")
    })
    
    
    ### Question: Social Distancing ####
    ########################################
    
    # this is a slight different model since it runs two curves, original, and shifted.
    calcModel_social_distance <- reactive({
        ## which scenario?
        if (input$select_social_distance == "base") {
            parameters <- c(b = 1/(4),
                            k = 1/(16))
        } else if (input$select_social_distance == "light") {
            parameters <- c(b = 1/(8),
                            k = 1/(16))
        } else { # heavy
            parameters <- c(b = 1/(12),
                            k = 1/(16))
        }
        
        ### SIR model
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0,
                   S_orig = 1,
                   I_orig = 1e-7,
                   R_orig = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -b*S*I 
                dI <- b*S*I - k*I
                dR <- k*I
                dS_orig <- -(1/4)*S_orig*I_orig 
                dI_orig <- (1/4)*S_orig*I_orig - k*I_orig
                dR_orig <- k*I_orig
                
                # return the rate of change
                list(c(dS, dI, dR, dS_orig, dI_orig, dR_orig))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters)
        data.frame(out)   # returns wide dataframe
    })    
    
    output$SIRplot_social_distance_SIR <- renderPlot({
        df <- calcModel_social_distance()
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "S" | 
               df$variable == "I" |
               df$variable == "R",], aes(x=time, y=value*default_popn*1e6, group=variable)) + 
                geom_line(aes(col=variable)) +
                xlab("Day") + ylab("# Persons")
    })
    

    output$SIRplot_social_distance <- renderPlot ({
        df <- calcModel_social_distance()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        df$cumulative_symptomatic_base_case <- (1-df$S_orig) * default_severe_symptomatic
        
        df <- melt(df, id=c("time"))   # make long
        if (input$select_social_distance != "base") {
            ggplot(df[df$variable == "cumulative_symptomatic" | df$variable == "cumulative_symptomatic_base_case",], aes(x=time, y=value*default_popn*1e6, group=variable)) + 
                geom_line(aes(col=variable)) +
                xlab("Day") + ylab("Cumulative # of persons over time")
        } else {
            ggplot(df[df$variable == "cumulative_symptomatic",], aes(x=time, y=value*default_popn*1e6, group=variable)) + 
                geom_line(aes(col=variable)) +
                xlab("Day") + ylab("Cumulative # of persons over time")
        }
    })    
    
    output$number_social_distance_sick <- renderText ({
        df <- calcModel_social_distance()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of symptomatic persons by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"]* default_popn*1e6,0), big.mark=","), "persons.")
    })
    
    output$number_social_distance_deaths <- renderText ({
        df <- calcModel_social_distance()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of deaths by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.")
    })
    
    
    ### Question: Timing ####
    ########################################
    
    # this is a slight different model since it runs two curves, original, and shifted.
    calcModel_timing <- reactive({
        parameters <- c(b = 1/(4),
                        k = 1/(16))
        
        ## which scenario?
        if (input$select_timing_start == "base") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(4-1), 7*5), value = c(0, 0), method = c("add", "add"))
        } else if (input$select_timing_start == "week4") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(4-1), 7*5), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_start == "week6") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(6-1), 7*7), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_start == "week8") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(8-1), 7*9), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_start == "week10") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(10-1), 7*11), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_start == "week12") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(12-1), 7*13), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_start == "week14") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(14-1), 7*15), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_start == "week16") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(16-1), 7*17), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else { # week18
            eventdat <- data.frame(var = c("Z"), time = c(7*(18-1), 7*19), value = c(1/12, 1/4), method = c("rep", "rep"))
        }
        
        ### SIR model
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0,
                   Z = 1/4,
                   S_orig = 1,
                   I_orig = 1e-7,
                   R_orig = 0 )
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -Z*S*I 
                dI <- Z*S*I - k*I
                dR <- k*I
                dZ <- 0
                dS_orig <- -b*S_orig*I_orig 
                dI_orig <- b*S_orig*I_orig - k*I_orig
                dR_orig <- k*I_orig
                
                # return the rate of change
                list(c(dS, dI, dR, dZ, dS_orig, dI_orig, dR_orig))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters, events = list(data=eventdat))
        data.frame(out)   # returns wide dataframe
    })    
    
    output$SIRplot_timing_SIR <- renderPlot({
        df <- calcModel_timing()
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "S" | 
                      df$variable == "I" |
                      df$variable == "R",], aes(x=time, y=value*default_popn*1e6, group=variable)) + 
            geom_line(aes(col=variable)) +
            xlab("Day") + ylab("# Persons")
    })
    
    output$number_timing_sick <- renderText ({
        df <- calcModel_timing()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of severe symptomatic persons by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"]* default_popn*1e6,0), big.mark=","), "persons.")
    })
    
    output$number_timing_deaths <- renderText ({
        df <- calcModel_timing()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of deaths by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.")
    })
    
    # this is a slight different model since it runs two curves, original, and shifted.
    calcModel_timing_duration <- reactive({
        parameters <- c(b = 1/(4),
                        k = 1/(16))
        
        ## which scenario?
        if (input$select_timing_duration == "week1") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(12-1), 7*12), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_duration == "week2") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(12-1), 7*13), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_duration == "week3") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(12-1), 7*14), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_duration == "week4") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(12-1), 7*15), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else if (input$select_timing_duration == "week5") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(12-1), 7*16), value = c(1/12, 1/4), method = c("rep", "rep"))
        } else { # week6
            eventdat <- data.frame(var = c("Z"), time = c(7*(12-1), 7*17), value = c(1/12, 1/4), method = c("rep", "rep"))
        }
        
        ### SIR model
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0,
                   Z = 1/4,
                   S_orig = 1,
                   I_orig = 1e-7,
                   R_orig = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -Z*S*I 
                dI <- Z*S*I - k*I
                dR <- k*I
                dZ <- 0
                dS_orig <- -b*S_orig*I_orig 
                dI_orig <- b*S_orig*I_orig - k*I_orig
                dR_orig <- k*I_orig
                
                # return the rate of change
                list(c(dS, dI, dR, dZ, dS_orig, dI_orig, dR_orig))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters, events = list(data=eventdat))
        data.frame(out)   # returns wide dataframe
    })    
    
    
    output$SIRplot_timing_SIR_duration <- renderPlot ({
        df <- calcModel_timing_duration()
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "S" | 
                      df$variable == "I" |
                      df$variable == "R",], aes(x=time, y=value*default_popn*1e6, group=variable)) + 
            geom_line(aes(col=variable)) +
            xlab("Day") + ylab("# Persons")
        
    })

    output$number_timing_sick_duration <- renderText({
        df <- calcModel_timing_duration()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of severe symptomatic persons by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"]* default_popn*1e6,0), big.mark=","), "persons.")
    })
    
    output$number_timing_deaths_duration <- renderText({
        df <- calcModel_timing_duration()
        df$cumulative_symptomatic <- (1-df$S) * default_severe_symptomatic
        paste("The cumulative number of deaths by the end of the outbreak is: ", format(round(df[nrow(df), "cumulative_symptomatic"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.")
    })
    
    
    ### Question: Seroprevalence ####
    ########################################
    
    calcModel_seroprevalence <- reactive({
        parameters <- c(b = 1/(4),
                        k = 1/(16))
        
        ## which scenario?
        if (input$select_seroprevalence == "short") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(11-1), 7*13), value = c(1/8, 1/4), method = c("rep", "rep"))
        } else if (input$select_seroprevalence == "longer") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(11-1), 7*16), value = c(1/8, 1/4), method = c("rep", "rep"))
        } else { # "long enough"
            eventdat <- data.frame(var = c("Z"), time = c(7*(11-1), 7*24), value = c(1/8, 1/4), method = c("rep", "rep"))
        }
        
        ### SIR model
        state <- c(S = 1,
                   I = 1e-7,
                   R = 0,
                   Z = 1/4,
                   S_orig = 1,
                   I_orig = 1e-7,
                   R_orig = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dS <- -Z*S*I 
                dI <- Z*S*I - k*I
                dR <- k*I
                dZ <- 0
                dS_orig <- -b*S_orig*I_orig 
                dI_orig <- b*S_orig*I_orig - k*I_orig
                dR_orig <- k*I_orig
                
                # return the rate of change
                list(c(dS, dI, dR, dZ, dS_orig, dI_orig, dR_orig))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters, events = list(data=eventdat))
        data.frame(out)   # returns wide dataframe
    })    
    
    
    output$SIRplot_seroprevalence <- renderPlot ({
        df <- calcModel_seroprevalence()
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "S" | 
                      df$variable == "I" |
                      df$variable == "R",], aes(x=time, y=value, group=variable)) + 
            geom_line(aes(col=variable)) +
            xlab("Day") + ylab("Proportion of the community population")
        
    })
    

    ### Question: heterogeneous ####
    ########################################
    
    calcModel_heterogeneous <- reactive({
        parameters <- c(b = 1/(8),
                        k = 1/(16))
        
        ## which scenario?
        if (input$select_heterogeneous == "base") {
            # do nothing.
            eventdat <- data.frame(var = c("Z"), time = c(7*(11-1), 7*14), value = c(0, 0), method = c("add", "add"))
        } else if (input$select_heterogeneous == "interv1") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(11-1), 7*14), value = c(1/12, 1/3), method = c("rep", "rep"))
        } else if (input$select_heterogeneous == "interv2") {
            eventdat <- data.frame(var = c("Z"), time = c(7*(11-1), 7*18), value = c(1/12, 1/3), method = c("rep", "rep"))
        } else { # "interv2"
            eventdat <- data.frame(var = c("Z"), time = c(7*(11-1), 7*22), value = c(1/12, 1/3), method = c("rep", "rep"))
        }

        ### SIR model
        state <- c(SA = 0.7,
                   IA = 1e-7,
                   RA = 0,
                   Z = 1/3,
                   SB = 0.3,
                   IB = 1e-7,
                   RB = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dSA <- -Z*SA*IA - b*SA*IB
                dIA <- Z*SA*IA + b*SA*IB - k*IA
                dRA <- k*IA
                dZ <- 0
                dSB <- -b*SB*IB - b*SB*IA
                dIB <- b*SB*IB + b*SB*IA - k*IB
                dRB <- k*IB
                
                # return the rate of change
                list(c(dSA, dIA, dRA, dZ, dSB, dIB, dRB))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 365, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters, events = list(data=eventdat))
        data.frame(out)   # returns wide dataframe
    })    
    
    output$SIRplot_heterogeneous <- renderPlot({
        df <- calcModel_heterogeneous()
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "IA" |
                      df$variable == "IB",], aes(x=time, y=value, group=variable)) + 
            geom_line(aes(col=variable)) +
            xlab("Day") + ylab("Proportion of the community population")        
    })
    
    output$number_heterogeneous_sick <- renderText({
        df <- calcModel_heterogeneous()
        df$cumulative_symptomaticA <- (1-df$SA) * default_severe_symptomatic
        df$cumulative_symptomaticB <- (1-df$SB) * default_severe_symptomatic
        paste("The cumulative number of severe symptomatic persons for group A: ", 
              format(round(df[nrow(df), "cumulative_symptomaticA"]* default_popn*1e6,0), big.mark=","), "persons.  And for group B: ",
              format(round(df[nrow(df), "cumulative_symptomaticB"]* default_popn*1e6,0), big.mark=","), "persons.")
    })
    
    output$number_heterogeneous_deaths <- renderText({
        df <- calcModel_heterogeneous()
        df$cumulative_symptomaticA <- (1-df$SA) * default_severe_symptomatic
        df$cumulative_symptomaticB <- (1-df$SB) * default_severe_symptomatic
        paste("The cumulative number of deaths for group A is: ", 
              format(round(df[nrow(df), "cumulative_symptomaticA"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.  And for group B: ",
              format(round(df[nrow(df), "cumulative_symptomaticB"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.")
    })

    
    ### Question: travel ####
    ########################################
    
    calcModel_travel <- reactive({
        
        ## which scenario?
        if (input$select_travel == "base") {
            parameters <- c(b = 1/(4),
                            k = 1/(16),
                            travp = 0)
        } else if (input$select_travel == "little") {
            parameters <- c(b = 1/(4),
                            k = 1/(16),
                            travp = 0.02)
        } else { # "more"
            parameters <- c(b = 1/(4),
                            k = 1/(16),
                            travp = 0.05)
        }
        
        # this starts infection 1 month later in B community.
        eventdat <- data.frame(var = c("IB"), time = c(7*4), value = c(1e-7), method = c("add"))
        
        
        ### SIR model
        state <- c(SA = 0.5,
                   IA = 1e-7,
                   RA = 0,
                   SB = 0.5,
                   IB = 0,
                   RB = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dSA <- -b*SA*IA - b*SA*IB*travp
                dIA <- b*SA*IA + b*SA*IB*travp - k*IA
                dRA <- k*IA
                dSB <- -b*SB*IB - b*SB*IA*travp
                dIB <- b*SB*IB + b*SB*IA*travp - k*IB
                dRB <- k*IB
                
                # return the rate of change
                list(c(dSA, dIA, dRA, dSB, dIB, dRB))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 365, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters, events = list(data=eventdat))
        data.frame(out)   # returns wide dataframe
    })    
    
    output$SIRplot_travel <- renderPlot({
        df <- calcModel_travel()
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "IA" |
                      df$variable == "IB",], aes(x=time, y=value, group=variable)) + 
            geom_line(aes(col=variable)) +
            xlab("Day") + ylab("Proportion of the community population")        
    })
    
    output$number_travel_sick <- renderText({
        df <- calcModel_travel()
        df$cumulative_symptomaticA <- (1-df$SA) * default_severe_symptomatic
        df$cumulative_symptomaticB <- (1-df$SB) * default_severe_symptomatic
        paste("The cumulative number of severe symptomatic persons for group A: ", 
              format(round(df[nrow(df), "cumulative_symptomaticA"]* default_popn*1e6,0), big.mark=","), "persons.  And for group B: ",
              format(round(df[nrow(df), "cumulative_symptomaticB"]* default_popn*1e6,0), big.mark=","), "persons.")
    })
    
    output$number_travel_deaths <- renderText({
        df <- calcModel_travel()
        df$cumulative_symptomaticA <- (1-df$SA) * default_severe_symptomatic
        df$cumulative_symptomaticB <- (1-df$SB) * default_severe_symptomatic
        paste("The cumulative number of deaths for group A is: ", 
              format(round(df[nrow(df), "cumulative_symptomaticA"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.  And for group B: ",
              format(round(df[nrow(df), "cumulative_symptomaticB"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.")
    })    
    
    
    ### Question: metapopulation ####
    ########################################
    
    calcModel_metapop <- reactive({
        
        ## which scenario?
        if (input$select_metapop == "base") {
            parameters <- c(b = 1/(4),
                            k = 1/(16),
                            travp = 0)
        } else if (input$select_metapop == "little") {
            parameters <- c(b = 1/(4),
                            k = 1/(16),
                            travp = 0.02)
        } else { # "more"
            parameters <- c(b = 1/(4),
                            k = 1/(16),
                            travp = 0.05)
        }
        
        # this starts infection 1 month later in B community.
        #eventdat <- data.frame(var = c("IB"), time = c(7*4), value = c(1e-7), method = c("add"))
        
        
        ### SIR model
        state <- c(SA = 0.5,
                   IA = 1e-7,
                   RA = 0,
                   SB = 0.5,
                   IB = 1e-7,
                   RB = 0)
        
        SIRmodel <- function(t, state, parameters) {
            with(as.list(c(state, parameters)),{
                # rate of change
                dSA <- -(1/5)*SA*IA - (1/5)*SA*IB*travp
                dIA <- (1/5)*SA*IA + (1/5)*SA*IB*travp - k*IA
                dRA <- k*IA
                dSB <- -b*SB*IB - b*SB*IA*travp
                dIB <- b*SB*IB + b*SB*IA*travp - k*IB
                dRB <- k*IB
                
                # return the rate of change
                list(c(dSA, dIA, dRA, dSB, dIB, dRB))
                
            }) # end with(as.list ...
        }
        
        times <- seq(0, 730, by = 0.01)
        
        out <- ode(y = state, times = times, func = SIRmodel, parms = parameters)
        data.frame(out)   # returns wide dataframe
    })    
    
    output$SIRplot_metapop <- renderPlot({
        df <- calcModel_metapop()
        df <- melt(df, id=c("time"))   # make long
        ggplot(df[df$variable == "IA" |
                      df$variable == "IB",], aes(x=time, y=value, group=variable)) + 
            geom_line(aes(col=variable)) +
            xlab("Day") + ylab("Proportion of the community population")        
    })
    
    output$number_metapop_sick <- renderText({
        df <- calcModel_metapop()
        df$cumulative_symptomaticA <- (1-df$SA) * default_severe_symptomatic
        df$cumulative_symptomaticB <- (1-df$SB) * default_severe_symptomatic
        paste("The cumulative number of severe symptomatic persons for city A: ", 
              format(round(df[nrow(df), "cumulative_symptomaticA"]* default_popn*1e6,0), big.mark=","), "persons.  And for city B: ",
              format(round(df[nrow(df), "cumulative_symptomaticB"]* default_popn*1e6,0), big.mark=","), "persons.")
    })
    
    output$number_metapop_deaths <- renderText({
        df <- calcModel_metapop()
        df$cumulative_symptomaticA <- (1-df$SA) * default_severe_symptomatic
        df$cumulative_symptomaticB <- (1-df$SB) * default_severe_symptomatic
        paste("The cumulative number of deaths for city A is: ", 
              format(round(df[nrow(df), "cumulative_symptomaticA"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.  And for city B: ",
              format(round(df[nrow(df), "cumulative_symptomaticB"] * default_popn*1e6 * default_deaths,0), big.mark=","), "persons.")
    })    
    
    
    ### Question: SEIR model ####
    ########################################
    
    calcModel_SEIR <- reactive({
      
        parameters <- c(b = 1/input$SEIR_contact_rate_fit,
                        a = 1/input$SEIR_latency_rate_fit,
                        g = 1/input$SEIR_infectious_period_fit)

      ### SIR model
      state <- c(S = 1,
                 E = 0,
                 I = 1e-7)
      
      SEIRmodel <- function(t, state, parameters) {
        with(as.list(c(state, parameters)),{
          # rate of change
          dS <- -b*S*I 
          dE <- b*S*I - a*E
          dI <- a*E - g*I
          #dR <- k*IA     (R(t) is just 1-(S+E+I))

          # return the rate of change
          list(c(dS, dE, dI))
          
        }) # end with(as.list ...
      }
      
      times <- seq(0, 730, by = 0.01)
      
      out <- ode(y = state, times = times, func = SEIRmodel, parms = parameters)
      data.frame(out)   # returns wide dataframe
    })    
    
    output$SEIRplot <- renderPlot({
      df <- calcModel_SEIR()
      df$R <- 1- (df$S+df$E+df$I)
      df <- melt(df, id=c("time"))   # make long
      ggplot(df, aes(x=time, y=value, group=variable)) + 
        geom_line(aes(col=variable)) +
        xlab("Day") + ylab("Proportion of the community population")        
    })
    
    calc_SEIR_fit_RMSE <- reactive({
      WA_cases$day <- seq(21,21+nrow(WA_cases)-1)
      WA_cases$infections_prop_ofEndWA <- ((WA_cases$confirmed_cases) / (WA_cases[nrow(WA_cases),"confirmed_cases"]))
      df <- calcModel_SEIR()
      df <- df[df$time <=80,]  # only up to day 80 to match when we have WA case data
      df$infections_prop_ofEndModel <- (1-df$S)/(1-df[nrow(df),"S"])
      fitdf <- merge (df, WA_cases, by.x="time", by.y="day", all.x=FALSE, all.y=TRUE)
      fitdf$diff <- fitdf$infections_prop_ofEndModel - fitdf$infections_prop_ofEndWA
      fitdf$diffsq <- fitdf$diff^2
      fitdf  # this is returned as dataframe
    })
    
    output$SEIR_fit_RMSE <- renderText({
      sqrt(mean(calc_SEIR_fit_RMSE()$diffsq))
    })   
    
    
    output$SEIRplot_fit <- renderPlot({
      WA_cases$day <- seq(21,21+nrow(WA_cases)-1)
      df <- calcModel_SEIR()
      df <- df[df$time <=80,]  # only up to day 80 to match when we have WA case data
      df$infections_prop_ofEnd <- (1-df$S)/(1-df[nrow(df),"S"])
      
      ggplot() + 
        geom_bar(data=WA_cases, aes(x = day, y=confirmed_cases/WA_cases[nrow(WA_cases),"confirmed_cases"]), stat = "identity") +
        xlab("Day") + ylab("Proportion of confirmed cases, up to Mar 20, 2020") +
        geom_line(data=df, aes(x=time, y=infections_prop_ofEnd), col="blue")
      # geom_line(data=df, aes(x=time, y=S))
    })
    
    
    ### runs two models with different parameters, but both with the same intervention 
    calcModel_SEIR_intervention <- reactive({
      
      parameters <- c(b1 = 1/3,
                      a1 = 1/2,
                      g1 = 1/20,
                      b2 = 1/2,
                      a2 = 1/6,
                      g2 = 1/22)
      
      # intervention timing and magnitude
      eventdat <- data.frame(var = c("Z"), time = c(7*(input$SEIR_intervetion_start-1), 
                                                      7*((input$SEIR_intervetion_start-1) + input$SEIR_intervention_duration)),
                               value = c(input$SEIR_intervention_magnitude, 1), method = c("rep", "rep"))

      ### SIR model
      state <- c(S1 = 1,
                 E1 = 0,
                 I1 = 1e-7,
                 Z = 1,
                 S2 = 1,
                 E2 = 0,
                 I2 = 1e-7)
      
      SEIRmodel <- function(t, state, parameters) {
        with(as.list(c(state, parameters)),{
          # rate of change
          dS1 <- -b1*S1*I1/Z
          dE1 <- b1*S1*I1/Z - a1*E1
          dI1 <- a1*E1 - g1*I1
          dZ = 0
          dS2 <- -b2*S2*I2/Z
          dE2 <- b2*S2*I2/Z - a2*E2
          dI2 <- a2*E2 - g2*I2
          #dR <- k*IA     (R(t) is just 1-(S+E+I))
          
          # return the rate of change
          list(c(dS1, dE1, dI1, dZ, dS2, dE2, dI2))
          
        }) # end with(as.list ...
      }
      
      times <- seq(0, 730, by = 0.01)
      
      out <- ode(y = state, times = times, func = SEIRmodel, parms = parameters, events = list(data=eventdat))
      data.frame(out)   # returns wide dataframe
    })    
    
    output$SEIRplot_intervention <- renderPlot({
      df <- calcModel_SEIR_intervention()
      df$R1 <- 1- (df$S1 + df$E1 + df$I1)
      df$R2 <- 1- (df$S2 + df$E2 + df$I2)
      df <- melt(df, id=c("time"))   # make long

      if (input$SEIR_intervention_whichModel == "Both") {
        ggplot(df[df$variable!="Z",], aes(x=time, y=value, group=variable)) + 
          geom_line(aes(col=variable)) +
          xlab("Day") + ylab("Proportion of the community population")        
      } else if (input$SEIR_intervention_whichModel == "First set of parameters") {
        ggplot(df[df$variable=="S1" | df$variable=="E1" | df$variable=="I1"| 
                    df$variable=="R1",], aes(x=time, y=value, group=variable)) + 
          geom_line(aes(col=variable)) +
          xlab("Day") + ylab("Proportion of the community population")        
      } else {
        ggplot(df[df$variable=="S2" | df$variable=="E2" | df$variable=="I2"| 
                    df$variable=="R2",], aes(x=time, y=value, group=variable)) + 
          geom_line(aes(col=variable)) +
          xlab("Day") + ylab("Proportion of the community population")        
      }
    })


}

# Run the application 
shinyApp(ui = ui, server = server)
