#!/usr/bin/env python

#from __future__ import unicode_literals
import actr
#import pyttsx3
#from nltk.tokenize import word_tokenize
from tokenizer import tokenize, TOK
import speech_recognition as sr
from naoqi import ALProxy
import sys
import rospy
import moveit_commander
import geometry_msgs.msg
from geometry_msgs.msg import PoseStamped, Pose
import moveit_msgs.msg
from moveit_msgs.msg import PlanningScene, ObjectColor
from math import pi
from std_msgs.msg import String
from moveit_commander.conversions import pose_to_list
import argparse
import time

global robot, scene, scene_pub
global right_arm_group, head, left_arm_group, both_arms_group
#global right_hand_group
#global gripper_pose_pub
#global eef, eef_link, touch_links

actr.load_act_r_model("ACT-R:INNER;DemoiScience;inner_model.lisp")
#actr.load_act_r_model("ACT-R:INNER;DemoSpecchio;mirror.lisp")

global response, response_time
response = ""
response_time = False

global IP, tts, asr

def init():
    global robot, scene, scene_pub, gripper_pose_pub
    global right_arm_group, right_hand_group, head, both_arms_group, left_arm_group

    #Initialize the move_group API
    moveit_commander.roscpp_initialize(sys.argv)
    rospy.init_node('move_group_py_grasp', anonymous=True)
    robot = moveit_commander.RobotCommander()
    scene = moveit_commander.PlanningSceneInterface()
          
    #Initialize the move group for the right arm and the right hand
    right_arm_group_name = "right_arm"
    right_arm_group = moveit_commander.MoveGroupCommander(right_arm_group_name)
    #right_hand_group_name = "right_hand"
    #right_hand_group = moveit_commander.MoveGroupCommander(right_hand_group_name)
    #display_trajectory_publisher = rospy.Publisher('/move_group/display_planned_path',moveit_msgs.msg.DisplayTrajectory,queue_size=20)
    head_name = "head"
    head = moveit_commander.MoveGroupCommander(head_name)
    both_arms_name = "both_arms"
    both_arms_group = moveit_commander.MoveGroupCommander(both_arms_name)
    left_arm_group_name = "left_arm"
    left_arm_group = moveit_commander.MoveGroupCommander(left_arm_group_name)



def AL_tts(string):
    global tts
    IP = "192.168.0.2"
    #tts = ALProxy("ALTextToSpeech", IP, 9559)
    tts = ALProxy("ALAnimatedSpeech", IP, 9559)
    config = {"bodyLanguageMode":"contextual"}
    #tts.setParameter("speed", 80)
    tts.say(string, config)
    #tts.say(string)
    if (string == "I will use my right arm group. I will try to move it."):
        go_to_start()
    elif (string == "I'm moving my arm"):
        go_to_final()
    elif (string == "So, people think I'm a robot, and I know I'm a robot with a mask, as the robot I'm seeing in the mirror. This robot is me?"):
        move_head_left()
        move_head_right()
        move_head_front()
    elif (string == "The mask I'm seeing is mine! I have a mask!"):
        go_to_rest()
    elif (string == "Oh, yes!"):
        go_to_heart()
    elif (string=="I think to be the robot in the mirror!"):
        go_to_rest()



def AL_speech_recognition():
    global asr, IP
    IP = "192.168.0.2"
    asr = ALProxy("ALSpeechRecognition", IP, 9559) 
    #memory = ALProxy("ALMemory", IP, 9559)
    #set the language of the speech recognition engine to English:
    asr.setLanguage("English")
    # Example: Adds "yes", "no" and "please" to the vocabulary
    vocabulary = ["give me the fork", "put the napkin on the fork", "yes I want"]
    asr.setVocabulary(vocabulary, False)
    # Or, if you want to enable word spotting:
    #asr.setVocabulary(vocabulary, True)
    
    # Start the speech recognition engine with user Test_ASR
    asr.subscribe("Test_ASR")
    time.sleep(5)
    # Stop the speech recognition engine with user Test_ASR
    asr.unsubscribe("Test_ASR")
    #scrivere codice per il recupero della stringa riconosciuta e preprocessarla per tornare le parole chiave 
    
         
def GAPI_speech_recognition() :
    actr.reset()
    # obtain audio from the microphone
    r = sr.Recognizer()
    with sr.Microphone() as source:
        r.adjust_for_ambient_noise(source)
        print("Say something!")
        audio = r.listen(source)
        text = r.recognize_google(audio, language="it-IT")
        #text = word_tokenize(text)
    # recognize speech using Google Speech Recognition
    try:
    # for testing purposes, we're just using the default API key
    # to use another API key, use `r.recognize_google(audio, key="GOOGLE_SPEECH_RECOGNITION_API_KEY")`
    # instead of `r.recognize_google(audio)`
        print("Google Speech Recognition thinks you said " + str(text))
    except sr.UnknownValueError:
        print("Google Speech Recognition could not understand audio")
    except sr.RequestError as e:
        print("Could not request results from Google Speech Recognition service; {0}".format(e))
    #actr.new_word_sound(text)
    #for word in text:
     #   actr.new_word_sound(word)
    #actr.run(30)
    return text


def demo_table() :
    actr.reset()
    #init()

    #...when manually setting the sentence (without syntetizer)
    text = "give fork"

    #...when using Aldebran proxy for speech recognition
    #text = AL_speech_recognition()

    #...when using Google Api (no Python 2.7)
    #text = GAPI_speech_recognition()

    string = tokenize(text)
    onset = 0
    actr.set_parameter_value(":sound-decay-time", 0.2)
    #actr.set_parameter_value(":save-audicon-history", True)
    actr.add_command("inner-speech-response",record_model_speech, "Inner speech model response")
    actr.monitor_command("output-speech","inner-speech-response")
    actr.install_device(["speech","microphone"])
    for word in string:
        if TOK.descr[word.kind] == "WORD" :
            print(str(word.txt))
            actr.new_word_sound(str(word.txt), onset)
            onset=onset+0.2
    actr.run(30)
    actr.remove_command_monitor("output-speech","inner-speech-response")
    actr.remove_command("inner-speech-response")
   

#go_to_rest_position
def go_to_rest():
    global right_arm_group, left_arm_group
    right_arm_joint_goal = right_arm_group.get_current_joint_values()
    left_arm_joint_goal = left_arm_group.get_current_joint_values()
    #print ("============== Joints value:", right_arm_joint_goal)
    right_arm_joint_goal[0] = 1.71192240715
    right_arm_joint_goal[1] = -0.124252557755
    right_arm_joint_goal[2] = 1.6643692255
    right_arm_joint_goal[3] = 0.131922245026
    right_arm_joint_goal[4] = -0.00771188735962

    left_arm_joint_goal[0] = 1.4480779171
    left_arm_joint_goal[1] = 0.159533977509
    left_arm_joint_goal[2] = -0.885106801987
    left_arm_joint_goal[3] = -0.452524423599 
    left_arm_joint_goal[4] = -0.897431850433

    right_arm_group.go(right_arm_joint_goal, wait=True)
    right_arm_group.stop()
    right_arm_group.clear_pose_targets()
    left_arm_group.go(left_arm_joint_goal, wait=True)
    left_arm_group.stop()
    left_arm_group.clear_pose_targets()


#go to start position
def go_to_heart():
    global both_arms_group
    both_arms_joint_goal = both_arms_group.get_current_joint_values()
    #print ("============== Joints value:", right_arm_joint_goal)
    both_arms_joint_goal[0] = -1.45881581306
    both_arms_joint_goal[1] = 1.52324295044
    both_arms_joint_goal[2] = -0.608990430832
    both_arms_joint_goal[3] = -1.34683513641
    both_arms_joint_goal[4] = -1.55551815033
    both_arms_joint_goal[5] = -0.12885427475
    both_arms_joint_goal[6] = -0.213223218918
    both_arms_joint_goal[7] = 1.74873816967
    both_arms_joint_goal[8] = 0.506213665009
    both_arms_joint_goal[9] = 1.17653608322
    both_arms_group.go(both_arms_joint_goal, wait=True)
    both_arms_group.stop()
    both_arms_group.clear_pose_targets()

#go to start position
def go_to_start():
    global right_arm_group
    right_arm_joint_goal = right_arm_group.get_current_joint_values()
    #print ("============== Joints value:", right_arm_joint_goal)
    right_arm_joint_goal[0] = 0.0
    right_arm_joint_goal[1] = -pi/4
    right_arm_joint_goal[2] = 0.0
    right_arm_joint_goal[3] = pi/4
    right_arm_joint_goal[4] = 0.0
    right_arm_group.go(right_arm_joint_goal, wait=True)
    right_arm_group.stop()
    right_arm_group.clear_pose_targets()

#go to final position
def go_to_final():
    global right_arm_group
    right_arm_joint_goal = right_arm_group.get_current_joint_values()
    #print ("============== Joints value:", right_arm_joint_goal)
    right_arm_joint_goal[0] = 0.107404537409
    right_arm_joint_goal[1] = -0.0498880034192
    right_arm_joint_goal[2] = 0.824640247544
    right_arm_joint_goal[3] = 1.44363582788
    right_arm_joint_goal[4] = 1.59567045112
    right_arm_group.go(right_arm_joint_goal, wait=True)
    right_arm_group.stop()
    right_arm_group.clear_pose_targets()

def move_head_left():
    global head
    head_joint_goal = head.get_current_joint_values()
    head_joint_goal[0] = -1.897534132
    head_joint_goal[1] = -0.191747665405
    head.go(head_joint_goal, wait=True)
    head.stop()
    head.clear_pose_targets()

def move_head_front():
    global head
    head_joint_goal = head.get_current_joint_values()
    head_joint_goal[0] = -0.159533977509
    head_joint_goal[1] = -0.0766990184784
    head.go(head_joint_goal, wait=True)
    head.stop()
    head.clear_pose_targets()

def move_head_right():
    global head
    head_joint_goal = head.get_current_joint_values()
    head_joint_goal[0] = 1.24559247494
    head_joint_goal[1] = -0.299126148224
    head.go(head_joint_goal, wait=True)
    head.stop()
    head.clear_pose_targets()

def demo_mirror() :
    init()
    actr.reset()
    actr.add_command("inner-speech-response",record_model_speech, "Inner speech model response")
    actr.monitor_command("output-speech", "inner-speech-response")
    actr.install_device(["speech","microphone"])
    actr.run(60)
    actr.remove_command_monitor("output-speech","inner-speech-response")
    actr.remove_command("inner-speech-response")

def record_model_speech (model, string):
    global response
    #response_time = actr.get_time(True)
    response = string
    print("GINOOOO ", str(response))
    #AL_tts(str(response))

demo_table()


