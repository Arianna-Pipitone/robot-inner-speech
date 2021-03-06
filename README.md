# robot-inner-speech
The repository was built as additional material of the paper entitled "What robots want? Hearing the inner voice of a robot" by Arianna Pipitone and Antonio Chella to the open access journal iScience from Cell Press.

The framework provides robots with inner speech skill. 
It is based on ACT-R architecture. 

Please, read the files in the INNER folder for details about the code of the model.
It contains the knowledge and the rules implementing the inner speech ability for robot in the cooperative scenario to set a table. 
You can add further declarative facts for extending knowledge.

You can try the inner speech model by just running ACT-R, or by integrating it to real robot.

Running inner speech model in ACT-R (without robot)
=====================================================

To run ACT-R inner speech model, you need:
   - `ACT-R architecture (standalone version) <http://act-r.psy.cmu.edu/software/>`
   - `Python 2.x or 3.x`
   - `Clozure common Lisp, as required in ACT-R standalone version`

Put the ACT-R folder in your preferred location path your-path.

Download the INNER folder and put it under the actr7.x folder of your ACT-R installation.

Launch ACT-R in the common lisp shell, like this:

    (load "..your-path.../act-r/actr7.x/load-act-r.lisp")
 
Run the Python code under INNER folder, like this (you have to write these lines in the Python interpreter shell):

    import inner_model
    inner_model.demo_table()
 
Now you can see the model working in your ACT-R console.

You can hear the inner speech dialogue by installing the SpeechRecognition Python library at

 <https://pypi.org/project/SpeechRecognition/>

After SpeechRecognition installation, when you run the model you can hear the inner dialogue produced by your machine.

Running inner speech model on real robot
==========================================

Prerequisites
-------------

Operative systems:

- Ubuntu 16.04 LTS or latest

- Windows with Windows Subsystem for Linux (WSL) with Ubuntu 16.04 LTS or latest 
(you can follow the official instructions available at <https://docs.microsoft.com/en-us/windows/wsl/install-manual> for enabling WSL)

For integrating the framework on Pepper robot, just ROS kinetic version is required (that is the ROS version for MoveIt! library), which runs on Ubuntu 16.04 LTS.

If you will use a different robot model, please verify the suitable ROS version, and the corresponding Ubuntu version.

1 Prepare the environment
-------------------------

- Install ACT-R architecture and the inner speech model

Please, follow the instructions of the previous case (running inner speech model (without robot))


- Install MoveIt! (which requires ROS)

Follow the instructions available at 

http://docs.ros.org/kinetic/api/moveit_tutorials/html/doc/getting_started/getting_started.html

This is a complete guide which allows you to install the complete framework for running ROS and enabling robot movements during interaction.
Keep attention about ROS version

-  Install the configuration file of your robot

You can find or built your own robot model by following the instuctions of the previous step in the section "Robot model and Robot state"

For Pepper, you can use the pepper_moveit_config package available at

<https://github.com/ros-naoqi/pepper_moveit_config>

Follow all the instructions to play with Pepper and try movements.

2 Compile the framework middleware
----------------------------------
The src package doesn't need any compilation so running rospack profile should be enough.

Please, refer to the file demo_isc.py into the src folder to see how implementing new robot movements and behaviours according to the inner turns.

Launch the middleware like this:

    rosrun robot-inner-speech demo_isc.py
    
and see your Pepper robot interacts to you while producing inner speech. 








