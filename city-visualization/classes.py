import numpy as np
import tensorflow as tf
from tkinter import *
from cmu_112_graphics import *
import time

class City:
    def __init__(self, x, y, total_w, total_h, rows, cols):
        self.subCities = []
        self.residents = []
        self.centerX = x
        self.centerY = y

        self.total_w = total_w
        self.total_h = total_h

        self.rows = rows
        self.cols = cols

        self.grid = np.zeros(shape = (self.rows, self.cols))
    
    def addResident(self, person):
        self.residents.append(person)

    def drawCity(self, canvas):
        for city in self.subCities:
            city.drawCity(canvas)

        for r in range(self.rows):
            for c in range(self.cols):
                color_vals = [int(255 / (1 + np.exp(-self.grid[r,c]))), int(255 / (1 + np.exp(-self.grid[r,c]))), int(255 / (1 + np.exp(-self.grid[r,c])))]
                color = f'#{str(hex(color_vals[0]))[2:]}{str(hex(color_vals[1]))[2:]}{str(hex(color_vals[2]))[2:]}'
                canvas.create_rectangle(r * self.total_w / self.rows, 
                                        c * self.total_h / self.cols, 
                                        (r+1) * self.total_w / self.rows, 
                                        (c+1) * self.total_h / self.cols,
                                        fill = color,
                                        outline = color)

        for person in self.residents:
            person.drawPerson(canvas)

    def suburbCreate(self, benchmark):
        if len(self.residents) > benchmark:
            pass


class Person:
    def __init__(self, locX, locY, jobX, jobY, travelCost, city, opt_method, jobCoeff, cityCoeff, densityCoeff):
        self.city = city
        self.jobx = jobX
        self.joby = jobY



        self.travelCost = travelCost
        self.jobCoeff = jobCoeff
        self.cityCoeff = cityCoeff
        self.densityCoeff = densityCoeff
        

        jobCost = lambda x,y: (1/travelCost)*(np.power(x - self.jobx, 2) + np.power(y - self.joby, 2))
        cityCost = lambda x,y: (1/travelCost) * ((np.power(x - self.city.centerX,2)) + (np.power(y - self.city.centerY,2)))

        tf_jobCost = lambda x,y: (1/travelCost)*(tf.pow(x - self.jobx, 2) + tf.pow(y - self.joby, 2))
        
        tf_cityCost = lambda x,y: (1/travelCost) * ((tf.pow(x - self.city.centerX,2)) + (tf.pow(y - self.city.centerY,2)))
        
        self.costfn_usable = lambda x,y: self.jobCoeff * jobCost(x, y) + self.cityCoeff * cityCost(x, y) + self.densityCoeff * self.concentrationLookup(x,y)

        if opt_method == "SGD":

            xVar = tf.Variable(locX)
            yVar = tf.Variable(locY)
            
            self.costfn = lambda: self.jobCoeff * tf_jobCost(xVar, yVar) + self.cityCoeff * tf_cityCost(xVar, yVar) + self.densityCoeff * self.concentrationLookup(xVar, yVar)
            opt = tf.keras.optimizers.SGD(learning_rate = 6.0)
            start = time.time()
            for _ in range(100):
                
                opt.minimize(self.costfn, var_list = [xVar, yVar])
                
            end = time.time()
            print(end - start)
            
            
            self.x = xVar.numpy()
            self.y = yVar.numpy()

            r_index = int(self.x // (self.city.total_w / self.city.rows))
            c_index = int(self.y // (self.city.total_h / self.city.cols))
            
            self.city.grid[r_index, c_index] += 1
        
        elif opt_method == "SOPT":
            fin_point = self.rand_samp_opt(20, 3, 50, 10)
            self.x = fin_point[0]
            self.y = fin_point[1]

            r_index = int(self.x // (self.city.total_w / self.city.rows))
            c_index = int(self.y // (self.city.total_h / self.city.cols))
            self.city.grid[r_index, c_index] += 1
        

    def rand_samp_opt(self, nm_points, j_choices, radius, n_iter):
        point_samp = []
        vector_cost = np.vectorize(lambda xy: self.costfn_usable(xy[0], xy[1]))
        
        for n in range(n_iter):
            if n == 0:
                for k in range(nm_points):
                
                    x_samp = np.random.uniform(0, self.city.total_w)
                    y_samp = np.random.uniform(0, self.city.total_h)
                    
                    point_samp.append((x_samp, y_samp))
                    
                    
                center_points = point_samp

            else:
                point_samp = np.empty(shape = [nm_points, nm_points], dtype=tuple)
                for k in range(nm_points):
                    for k_inner in range(nm_points):
                        cur_x = center_points[k][0]
                        cur_y = center_points[k][1]
                        r = np.random.uniform(0, radius)
                        degree = np.random.uniform(0, 360)

                        x_samp = cur_x + r * np.cos(degree)
                        y_samp = cur_y + r * np.sin(degree)
                        

                        point_samp[k,k_inner] = (x_samp, y_samp)
                
                costs = vector_cost(point_samp)
                
                argsort_costs = np.argsort(costs, axis = 1)
                
                center_points = [point_samp[row][argsort_costs[row,0]] for row in range(argsort_costs.shape[0])]

                

        costs = np.array(list(map(lambda xy: self.costfn_usable(xy[0], xy[1]), center_points)))
        argsort_costs = np.argsort(costs)
        fin_point = center_points[argsort_costs[0]]
        
        return fin_point
                      
    def concentrationLookup(self, x, y):
        
        if y > self.city.total_h:
            y = self.city.total_h
        elif y < 0:
            y = 0

        if x > self.city.total_w:
            x = self.city.total_h
        elif x < 0:
            x = 0

        
        
        r = x / (self.city.total_w / self.city.rows) # no quantizing
        c = y / (self.city.total_h / self.city.cols) # no quantizing
        r1, c1 = int(r), int(c) # lower bounds                                                                 
        r2, c2 = r1 + 1, c1 + 1 # upper bounds
        w_r2, w_c2 = r - r1, c - c1
        w_r1, w_c1 = 1.0 - w_r2, 1.0 - w_c2
        
        # Assume constant boundary conditions.
        c2 = tf.clip_by_value(c2, 0, self.city.grid.shape[1]-1)
        c1 = tf.clip_by_value(c1, 0, self.city.grid.shape[1]-1)
        r2 = tf.clip_by_value(r2, 0, self.city.grid.shape[0]-1)
        r1 = tf.clip_by_value(r1, 0, self.city.grid.shape[0]-1)
        
        if c2 > self.city.grid.shape[1] - 1:
            c2 = self.city.grid.shape[1]-1
        if c2 < 0:
            c2 = 0
        if c1 > self.city.grid.shape[1] - 1:
            c1 = self.city.grid.shape[1]-1
        if c1 < 0:
            c1 = 0
        if r2 > self.city.grid.shape[0] - 1:
            r2 = self.city.grid.shape[0]-1
        if r2 < 0:
            r2 = 0
        if r2 > self.city.grid.shape[0] - 1:
            r2 = self.city.grid.shape[0]-1
        if r2 < 0:
            r2 = 0
        
        return w_r1*w_c1*self.city.grid[r1, c1] + w_r2*w_c2*self.city.grid[r2,c2] + w_r1*w_c2*self.city.grid[r1,c2] + w_r2*w_c1*self.city.grid[r2, c1]
        

    def drawPerson(self, canvas):
        
        canvas.create_rectangle(self.x, self.y, self.x+1, self.y+1, fill = "red", outline = 'red')
        canvas.create_rectangle(self.jobx, self.joby, self.jobx+1, self.joby+1, fill = "blue", outline = 'blue')
        canvas.create_line(self.x, self.y, self.jobx, self.joby)
        canvas.create_text(self.jobx-3, self.joby, text = str(round(self.travelCost, 2)), font = 'ComicSansMS 5')
        '''
        
        if self.travelCost < 2:
            color = 'red'
        elif self.travelCost < 4:
            color = 'blue'
        elif self.travelCost < 6:
            color = 'purple'
        else:
            color = 'green'
        canvas.create_rectangle(self.x, self.y, self.x+1, self.y+1, fill = color, outline = color)
        '''
        

class Landmark:
    def __init__(self, locx, locy):
        self.x = locx
        self.y = locy



class Park(Landmark):
    def __init__(self, locx, locy):
        pass

class School(Landmark):
    def __init__(self, locx, locy):
        pass

class Family(Person):
    def __init__(self, people):
        self.family = people














