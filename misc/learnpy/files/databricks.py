
import requests
valid_response  = False 
username = "mutaz.abughazaleh@microsoft.com"
course_name = "Databricks Delta"
url = 'http://server1.databricks.training:9099/api/1/validate'
valid = True
try:
  r = requests.post(url, json={"userEmail": username, "courseName": course_name}, timeout=15)
  valid = r.status_code == 200
except requests.exceptions.Timeout as e:
  pass

if valid:
  valid_response  = True
  print(r)
else:
  print("*" * 80)
  print("User {} is not authorized to run this course.".format(username))
  print("Perhaps you purchased the course with another email address.")
  print("*" * 80)
  raise Exception("Unauthorized.")
