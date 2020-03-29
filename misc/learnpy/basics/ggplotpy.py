#using ggplot in python 

import pandas as pd
from ggplot import * 
import numpy as np 



height = np.round(np.random.normal(1.75, .20, 5000), 2)
weight = np.round(np.random.normal(60.32, 15, 5000), 2)


df1 = pd.DataFrame(data=np.array([height,weight]))

# pd.DataFrame( np.ar)

# ggplot(aes(x=height, y=weight)) +\
# geom_point()