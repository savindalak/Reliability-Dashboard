library(data.table)
library(XML)
library(curl)
library(dplyr)
library(ggplot2)
library(viridis) 
library(shinydashboard)
library(shiny)
library(ggpubr)
library(gridExtra)
library(shinyWidgets)
library(shinythemes)
library(fitdistrplus)


21#=====================Load file ===============================================================
file<-read.csv('C:\\Users\\savinda\\Documents\\Computer Science\\R data anatytics\\Machine_data_Aitken.csv')
names(file)
file$Date<- as.Date(file$Date,format='%d/%m/%Y')
#====================Add columns===============================================================
file<-file%>%mutate(MTBF=No_of_hours_per_wk/No_of_breakdowns)%>%mutate(
  MTTR=Repair_time_per_wk/No_of_completed_repairs)%>%mutate(
    Failure_rate=No_of_breakdowns/No_of_hours_per_wk)%>%mutate(
      Safety_issues_per_machine_hr=Safety_issues/No_of_hours_per_wk)%>%mutate(
        cum_repair_cost=cumsum(Repair_cost))%>%mutate(
          cum_budget=cumsum(Budgeted_cost))%>%mutate(
            ave_no_hours_before_failure =No_of_hours_per_wk/No_of_breakdowns)
names(file)
mid_num<-round(length(file$Date)/2,digits = 0)

#====================Failure rate graph========================================================
failure_rate_start<-mean(file$Failure_rate[1:mid_num])
failure_rate_end<-mean(file$Failure_rate[mid_num:length(file$Date)])

failure_rate_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=Failure_rate))+geom_point()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+geom_path()+ggtitle(
    'Failure Rate Monitoring Chart')+ylab('Failure Rate (Failures / Hr)')+geom_segment(
      aes(x=Date[1],xend=Date[mid_num],y=failure_rate_start,yend=failure_rate_start
      ),linetype="3313",col='red',size=1)+geom_segment(
        aes(x=Date[mid_num],xend=Date[length(Date)],y=failure_rate_end,yend=failure_rate_end
        ),linetype="3313",col='blue',size=1)+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            fill = "#BFD5E3",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
failure_rate_plot

#====================MTBF Graph================================================================

MTBF_start = mean(file$MTBF[1:mid_num])
MTBF_end=mean(file$MTBF[mid_num:length(file$Date)])

MTBF_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=MTBF))+geom_point()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+geom_path()+ggtitle(
    'MTBF Monitoring chart')+ylab('MTBF (Hours) ')+geom_segment(
      aes(x=Date[1],xend=Date[mid_num],y=MTBF_start,yend=MTBF_start),linetype="3313",col='red'
      ,size=1)+geom_segment(
        aes(x=Date[mid_num],xend=Date[length(Date)],y=MTBF_end,yend=MTBF_end
        ),linetype="3313",col='blue',size=1)+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            fill = "#BFD5E3",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
MTBF_plot

x=file$Date[1:mid_num]
x
#===========================MTTR Graph=========================================================

MTTR_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=MTTR,fill=(MTTR>10)))+geom_col()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+ggtitle(
    'Weekly MTTR Distribution')+ylab('MTTR in Hours')+geom_hline(
      aes(yintercept=mean(MTTR)),color="blue", linetype="dashed", size=1)+scale_fill_manual(
        values=c("#1CE015", "#F96C0A"))+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
MTTR_plot

#===========================Machine working hours graph=======================================

working_hours_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=No_of_hours_per_wk,fill=(
  No_of_hours_per_wk<1200)))+geom_col()+theme(
    axis.text.x = element_text( angle = 90, hjust =1,vjust=0))+ggtitle(
      'Weekly Machine Working Hours')+ylab('Working Hours')+guides(fill=guide_legend(
        title='Working hours<1200'))+scale_fill_manual(values=c("blue", "red"))+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            fill = "#F6F6F0",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
working_hours_plot
#==========================Safety issues chart================================================

safety_issues_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=Safety_issues_per_machine_hr))+geom_point(color='yellow',size=3)+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+geom_path(size=1)+ggtitle(
    'Safety Monitoring Chart')+ylab('Safety issues per machine hr')+theme(
      plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
        fill = "#FC5A4F",colour = "black",size =1, linetype = "solid"
      ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
safety_issues_plot

#==========================Employee Attendance Level==========================================

attendance_plot<-file%>%arrange(Date)%>%ggplot(aes(x=Date,y=Attendance_percentage))+geom_point()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+ggtitle(
    'Weekly Employee Attendance Level')+ylab('% Attendance Level')+geom_path()+geom_hline(
      aes(yintercept=mean(Attendance_percentage)),color="blue", linetype="dashed", size=1
    )+theme(plot.title =element_text(
      face = 'bold',size = 16),panel.background = element_rect(
        fill = "#9DFC29",colour = "black",size =1, linetype = "solid"
      ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
attendance_plot

#==========================Cost vs Budget plot==================================================

repair_cost<- file%>%ggplot(aes(x=Date))+geom_line(
  aes(y=cum_budget,col='Budgeted Cost'),size=1)+geom_line(aes(
    y=cum_repair_cost,col='Repair Cost'),size=1)+ylab('Cost') + ggtitle(
      'Cumulative actual cost vs Cumulative Budgeted cost')+theme(
        legend.title = element_blank())+scale_colour_manual(values=c("green","red"))+theme(
          plot.title =element_text(face = 'bold',size = 16),panel.background = element_rect(
            fill = "#ECECD3",colour = "black",size =1, linetype = "solid"
          ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
repair_cost
#==========================Weibull Reliability plot========================================================

wbl_param<-fitdist(file$ave_no_hours_before_failure,'weibull')
shape<-wbl_param$estimate['shape']
scale<-wbl_param$estimate['scale']

tail<-tail(file$ave_no_hours_before_failure,n=1)
t<-(1:tail)
a<- exp(-(t/scale)^shape)
pdf<- (shape/scale)*(t/scale)^(shape-1)*exp(-(t/scale)^shape)

weibull_plot<-ggplot(mapping=aes(x=t,y=a))+geom_line(size=1)+ylab(
  'Density')+xlab('hours')+ggtitle('Weibull Reliability plot')+theme(plot.title =element_text(
    face = 'bold',size = 16),panel.background = element_rect(
      fill = "#BFD5E3",colour = "black",size =1, linetype = "solid"
    ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))

weibull_pdf<-ggplot(mapping=aes(x=t,y=pdf))+geom_line()
weibull_pdf

tab <- c(Shape_factor = round(shape,digits = 2),
         scale_factor = round(scale,digits = 2))
row_names<-c('Shape','Scale')

p_tab <- tableGrob(tab,rows = row_names)
grid.arrange(weibull_plot, p_tab, ncol =2,widths=c(4,1))

weibull_final<-function(){
  
  p_tab <- tableGrob(tab,rows = row_names)
  grid.arrange(weibull_plot, p_tab, ncol =2,widths=c(4,1))
}

weibull_final()

#=====================Cost per cycle graph====================================================

cost_per_cycle_plot<-file%>%mutate(cost_per_box= Repair_cost/No_of_boxes)%>%ggplot(
  aes(x=Date,y=cost_per_box,fill=(cost_per_box>100)))+geom_col()+geom_hline(
    aes(yintercept=mean(cost_per_box)),color="blue", linetype="dashed", size=1)+ylab(
      'Cost per cyple (LKR)')+scale_fill_manual(values=c("#28E5D2", "#E52847"),labels=c(
        'Under 100','Over 100'),name='')+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            colour = "black",size =1, linetype = "solid"),panel.grid.major = element_line(
              size = 0.5, linetype = 'solid',colour = "white"))+ggtitle('Repair cost per cycle')
cost_per_cycle_plot    

#======================Inherent Availability=====================================================

inherent_availability_plot<-file%>%mutate(inherent_availability=MTBF/(MTTR+MTBF))%>%ggplot(aes(x=Date,y=inherent_availability))+geom_point()+theme(
  axis.text.x = element_text(  angle = 90, hjust =1,vjust=0))+ggtitle(
    'Inherent Availability of the fleet')+ylab('% Inherent Availability')+geom_path()+geom_hline(
      aes(yintercept=mean(inherent_availability)),color="blue", linetype="dashed", size=1
    )+theme(plot.title =element_text(
      face = 'bold',size = 16),panel.background = element_rect(
        fill = "#9DFC29",colour = "black",size =1, linetype = "solid"
      ),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"))
inherent_availability_plot

#======================Machine efficiency======================================================

boxes_per_hour<- file%>%mutate(no_of_boxes_per_hour=No_of_boxes/No_of_hours_per_wk)%>%ggplot(aes(
  x=Date,y=no_of_boxes_per_hour,fill=(no_of_boxes_per_hour<7)))+geom_col()+theme(
    axis.text.x = element_text( angle = 90, hjust =1,vjust=0))+ggtitle(
      'Boxes handled per machine hour')+ylab('Boxes per hour')+guides(fill=guide_legend(
        title='boxes per hr < 7'))+scale_fill_manual(values=c("blue", "red"))+theme(plot.title =element_text(
          face = 'bold',size = 16),panel.background = element_rect(
            fill = "#F6F6F0",colour = "black",size =1, linetype = "solid"))




#================Render Dash board with shiny==================================================


ui<-navbarPage(theme = shinytheme("cyborg"),title = div(img(src='logo_aitken.png',height=50,width=150, style= 'margin-right:600px'),'RELIABILITY DASHBOARD'),
               
               #tags$div(tags$img(src='logo_aitken.png', width = 150, height = 50, style="float:left; margin-left: 5px; margin-right: 5px; margin-top: 0px")), 
               
               #img(src="C:\\Users\\savinda\\Documents\\Computer Science\\R data anatytics\\logo_aitken.png", align = "left",height='50px',width='50px'),
               
               
               dashboardBody(
                 
                 fluidRow(
                   
                   column(4,plotOutput('plot1',height = 280),style='padding:10px'),
                   column(4,plotOutput('plot2',height = 280),style='padding:10px'),
                   column(4,plotOutput('plot7',height = 280),style='padding:10px'),
                   
                   
                 ),
                 
                 fluidRow(
                   column(4,plotOutput('plot3',height = 280),style='padding:10px'),
                   column(4,plotOutput('plot4',height = 280),style='padding:10px'),
                   column(4,plotOutput('plot8',height = 280),style='padding:10px'),
                   
                   
                 ),
                 
                 fluidRow(
                   column(4,plotOutput('plot5',height = 280),style='padding:10px'),
                   column(4,plotOutput('plot6',height = 280),style='padding:10px'),
                   column(4,plotOutput('plot9',height = 280),style='padding:10px'),
                   
                 ),
               )
)



server<- function(input,output){
  
  output$plot1<- renderPlot({
    
    failure_rate_plot
    
    
  })
  output$plot2<- renderPlot({
    
    repair_cost
    
  })
  
  output$plot3<- renderPlot({
    
    MTTR_plot
    
  })
  
  output$plot4<- renderPlot({
    
    boxes_per_hour
    
  })
  
  output$plot5<- renderPlot({
    
    safety_issues_plot
    
  })
  
  output$plot6<- renderPlot({
    
    inherent_availability_plot
    
  })
  
  output$plot7<- renderPlot({
    
    cost_per_cycle_plot
    
  })
  
  output$plot8<- renderPlot({
    
    MTBF_plot
    
  })
  
  output$plot9<- renderPlot({
    
    weibull_final()
    
  })
}

app<-shinyApp(ui,server)
app

