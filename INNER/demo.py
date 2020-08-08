import actr
import pyttsx3
from tokenizer import tokenize, TOK

#from naoqi import ALProxy

actr.load_act_r_model("ACT-R:INNER;DemoiScience;inner_model.lisp")

global response, response_time
response = ''
response_time = False

def record_model_speech (model, string):
    global response,response_time
    #response_time = actr.get_time(True)
    response = string
    speech(response)
    
def record_model_motor (model, key):
    global response,response_time
    #response_time = actr.get_time(True)
    response = key
    speech(response)
   
def speech(response):
    engine = pyttsx3.init()
    engine.setProperty('rate', 125) 
    engine.say(response)
    engine.runAndWait()
    engine.stop()
    
def speech_recognition() :
    #actr.reset()
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
        #actr.new_word_sound(word)
    #actr.run(30)
    return str(text)


def demo_table() :
    actr.reset()
    #init()

    #...when manually setting the sentence (without syntetizer)
    text = "put napkin near table"

    #...when using Aldebran proxy for speech recognition
    #text = AL_speech_recognition()

    #...when using Google Api (no Python 2.7)
    #text = GAPI_speech_recognition()

    string = tokenize(text)
    onset = 0
    actr.set_parameter_value(":sound-decay-time", 0.4) #0.3 thread 1
    #actr.set_parameter_value(":save-audicon-history", True)
    actr.add_command("inner-speech-response",record_model_speech, "Inner speech model response")
    actr.monitor_command("output-speech","inner-speech-response")
    actr.install_device(["speech","microphone"])
    actr.add_command("motor-response",record_model_motor, "Motor model response")
    actr.monitor_command("pech","motor-response")
    for word in string:
        if TOK.descr[word.kind] == "WORD" :
            print(str(word.txt))
            actr.new_word_sound(str(word.txt), onset)
            onset=onset+1.5 #0.4 thread 1
    actr.run(30)
    actr.new_word_sound("yes", 15)
    actr.remove_command_monitor("output-speech","inner-speech-response")
    actr.remove_command("inner-speech-response")

   

def AL_tts():
    IP = "192.168.0.2"
    tts = ALProxy("ALTextToSpeech", IP, 9559)
    tts.say("Hello my name is Pepper")

#AL_tts()
#demo_table()
