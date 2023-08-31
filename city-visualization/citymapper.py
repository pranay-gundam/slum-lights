from tkinter import *

from numpy import newaxis
from cmu_112_graphics import *
from classes import *
import time


#from https://www.cs.cmu.edu/~112/notes/notes-graphics.html#customColors
def rgbString(red, green, blue):
    return "#%02x%02x%02x" % (red, green, blue)




class StartMode(Mode):
    def appStarted(mode):
        mode.isPaused = False
        mode.City = City(mode.width//2, mode.height//2, mode.width, mode.height, 100, 50)
        mode.pop = 0
        
    def redrawAll(mode, canvas):
        mode.City.drawCity(canvas)
        canvas.create_text(mode.width//2, 10, text = f'Population: {mode.pop}', fill = 'black', font = 'ComicSansMS 10 bold')
        
    def keyPressed(mode, event):
                
        # If we click enter let's pause the simulation
        if event.key == "Enter":
            mode.isPaused = not mode.isPaused

    def timerFired(mode):
        if not mode.isPaused:
            mode.pop += 1
            newXinit = np.random.uniform(low = 0, high = mode.width+1)
            newYinit = np.random.uniform(low = 0, high = mode.height+1)
            
            #newX = tf.Variable(newXinit, trainable=True)
            #newY = tf.Variable(newYinit, trainable=True)
            
            jobX = np.random.normal(loc =  mode.width/2, scale = 50)
            jobY = np.random.normal(loc =  mode.height/2, scale = 50)
            #newPerson = Person(newXinit, newYinit, jobX, jobY, 0.1, mode.City)

            travelCost = np.random.chisquare(df = 2.5)+1

            newPerson = Person(jobX, jobY, jobX, jobY, travelCost, mode.City, "SGD", 0.1, 0.01, 100)
            #newPerson = Person(newXinit, newYinit, jobX, jobY, travelCost, mode.City, "SOPT")
            #newPerson = Person(newXinit, newYinit, jobX, jobY, 0.1, mode.City)
            mode.City.addResident(newPerson)
            #newX = np.sample([1:mode.w])
            #newY = np.sample([1:mode.h])
            
            if mode.pop % 100 == 0:
                # make the city reorganize, this is to reflect the passage of time and how people move within a city
                pass
                #mode.City.re_opt()


class CitySim(ModalApp):
    def appStarted(app):
        app.startMode = StartMode()
        app.setActiveMode(app.startMode)


app = CitySim(width=1280, height=666)
