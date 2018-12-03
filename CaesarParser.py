"""
A file for parsing a bunch of text files that were produced from Northwestern University CTECs
written poorly by Matt Nicholson
"""

import json
import re
import traceback

def count_occurrences(word, sentence):
    """
    a helper for counting the number of time a word appears in a sentence
    """
    return sentence.lower().split().count(word)

def read_demographics(course_dict, lines, index):
    """
    a function for parsing the specific part that asks about demographics
    
    @params:
    course_dict - a dictionary with all of the information about a course in it
    lines - a list containing each line of a text file
    index - an integer the line number you are looking at
    
    @returns course_dict,  index
    
    """        
    demo_done = 0
    
    sb = {}
    your_class = {}
    reasons = {}
    interests = {}
    
    
    consecutive_newlines = 0
    
    i = index
    
    while consecutive_newlines < 3 and i < len(lines):
        if "Graphs illustrating the information" in lines[i]:
            consecutive_newlines = 0
            if demo_done == 0:
                i += 2
                while "Your Class\n" not in lines[i]:
                    words = re.split(r'\s{2,}', lines[i])
                    sb[words[0]] = int(words[1].strip())
                    i += 1
                demo_done += 1
                
            elif demo_done == 1:
                i += 2
                while "What is your reason" not in lines[i]:
                    words = re.split(r'\s{2,}', lines[i])
                    your_class[words[0]] = int(words[1].strip())
                    i += 1
                demo_done += 1
                
            elif demo_done == 2:
                i += 2
                while "Respond" not in lines[i]:
                    words = re.split(r'\s{2,}', lines[i])
                    #print(words)
                    reasons[words[0]] = int(words[1].strip())
                    i += 1
                demo_done += 1
            elif demo_done == 3:
                i += 2
                while lines[i] != "\n":
                    words = re.split(r'\s{2,}', lines[i])
                    # print(words)
                    interests[words[0]] = int(words[1].strip())
                    i += 1
                demo_done += 1
        else:
            if lines[i] == "\n":
                consecutive_newlines += 1
        
        i += 1
        
    course_dict["school_breakdown"] = sb
    course_dict["reason_for_taking_course"] = reasons
    course_dict["student_years"] = your_class
    course_dict["interest_before"] = interests
        
    return course_dict, i


def read_number_ratings(course_dict, lines, index):
    """
    a function for parsing the specific part that asks about course ratings and stuff
    
    """
    done = 0
    instruction_rating = ""
    course_rating = ""
    learned = ""
    how_hard = ""
    instructor_interest = ""
    time = {}
    
    consecutive_newlines = 0
    
    i = index
    
    while done < 5:
        if "Graphs illustrating the information" in lines[i]:
            if done == 0:
                i += 3
                words = re.split(r'\s{2,}', lines[i])
                instruction_rating = float(words[1].strip())
                # print(instruction_rating)
                done += 1
            elif done == 1:
                i += 3
                words = re.split(r'\s{2,}', lines[i])
                course_rating = float(words[1].strip())
                # print(course_rating)
                done += 1
            elif done == 2:
                i += 3
                words = re.split(r'\s{2,}', lines[i])
                learned = float(words[1].strip())
                # print(learned)
                done += 1
            elif done == 3:
                i += 3
                words = re.split(r'\s{2,}', lines[i])
                how_hard = float(words[1].strip())
                # print(how_hard)
                done += 1
                
            elif done == 4:
                i += 3
                words = re.split(r'\s{2,}', lines[i])
                instructor_interest = float(words[1].strip())
                # print(instructor_interest)
                done += 1
            elif done == 5:
                i += 2
                while lines[i] != "ESSAY QUESTIONS\n":
                    words = re.split(r'\s{2,}', lines[i])
                    time[words[0]] = int(words[1].strip())
                    i += 1
                done += 1
        i += 1
    

    course_dict["rating_of_instruction"] = instruction_rating
    course_dict["rating_of_course"] = course_rating
    course_dict["how_much_learned"] = learned
    course_dict["how_hard"] = how_hard
    course_dict["instructor_interest"] = instructor_interest
    course_dict["time_spent"] = time
    
    # print(course_dict.keys())
    
    return course_dict, i

def read_in_course_info(lines):
    """
    a function for reading in all of the information for a course
    """
    course_info = {}
    
    
    num_lines = len(lines)
    at_comments = False
    at_number_questions = False
    at_demographics = False
    
    # sorry about hitting everyone with the gender binary, Brent told me to
    she_her_hers_count = 0
    he_him_his_count = 0
    
    comments = []
    
    ii = 0
    
    while ii < num_lines:
        if "DEMOGRAPHICS" in lines[ii]:
            at_comments = False
            course_info, ii = read_demographics(course_info, lines, ii)
                    
        elif at_comments:
            comments.append(lines[ii].strip())
            
            she_her_hers_count += count_occurrences("she", lines[ii])
            she_her_hers_count += count_occurrences("her", lines[ii])
            she_her_hers_count += count_occurrences("hers", lines[ii])
            
            he_him_his_count += count_occurrences("he", lines[ii])
            he_him_his_count += count_occurrences("him", lines[ii])
            he_him_his_count += count_occurrences("his", lines[ii])
            
            
        elif "Student Report for" in lines[ii]:
            # they are all funky, so I standardized some
            course_info["course_name"] = lines[ii].split("Student Report for")[1].split("(")[0].strip()
            course_info["instructor"] = lines[ii].split("(")[1].split(")")[0].strip()

        elif "Project Audience" in lines[ii]:
            course_info["students_enrolled"] = int(lines[ii].split("Project Audience ")[1].strip())

        elif "Responses Received" in lines[ii]:
            course_info["responses_recieved"] = int(lines[ii].split("Responses Received ")[1].strip())

        elif "Comments\n" == lines[ii]:
            at_comments = True
            at_number_questions = False

        elif "COURSE QUESTIONS" in lines[ii]:
            at_number_questions = True
            course_info, ii = read_number_ratings(course_info, lines, ii)
            # print(course_info.keys())
            
        ii += 1
    
    course_info["comments"] = comments
    
    if she_her_hers_count > he_him_his_count:
        course_info["instructor_gender"] = "F"
    elif she_her_hers_count == he_him_his_count:
        course_info["instructor_gender"] = "unknown"
    else:
        course_info["instructor_gender"] = "M"
    
    return course_info


def do_the_whole_thing(which):
    """
    this function is the main driver for parsing one CTEC text file
    
    @params: which - a string saying which file you want to parse
    
    @returns: None
    
    @modifies: writes a file to disk
    """
    with open("CTECs/{}.txt".format(which), "r", encoding="utf8") as f:
        all_lines = f.readlines()
    
    total_num_lines = len(all_lines)

    jj = 0

    with open('data.json', 'r') as fp:
        all_courses = json.load(fp)

    start_points = []

    # this is dumb, but it's gonna work and really it only needs to work one time

    while jj < total_num_lines:

        if 'Northwestern\n' in all_lines[jj]:
            start_points.append(jj)
            # print("found start point at {}".format(jj))
        jj += 1
    
    num_start_points = len(start_points)
    kk = 0
    all_courses[which] = []

    while kk+1 < num_start_points:
        all_courses[which].append(read_in_course_info(all_lines[start_points[kk]:start_points[kk+1]]))
        print("done with course {}".format(kk))
        kk += 1

    with open('data.json', 'w') as fp:
        json.dump(all_courses, fp, indent=4)
        

def main():
    with open('data.json', 'w') as fp:
        json.dump({}, fp, indent=4)
        
    depts = ["BME", "EECS", "English", "Gender Studies", "Linguistics", "LOC", "Music", "Spanish"]

    for dept in depts:
        try:
            do_the_whole_thing(dept)
        except Exception as e:
            print("+"*20)
            print(dept)
            print("Something messed up here: {}".format(e))
            traceback.print_exc()
            print("+"*20)


if __name__ == "__main__":
    main()