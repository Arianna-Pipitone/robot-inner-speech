# robot-inner-speech
A framework to provide robots with inner speech skill. 
It is based on ACT-R architecture, which is integrated to typical robot's routines, as ROS, text to speech, speech to text. 

You can try the inner speech model by just running ACT-R, or by integrating it to real robot.

Running just inner speech model in ACT-R
----------------------------------------
To run ACT-R inner speech model, you need:
   - `ACT-R architecture (standalone version) <http://act-r.psy.cmu.edu/software/>`
   - `Python 2.x or 3.x`
   - `Clozure common Lisp, as required in ACT-R standalone version`

    Put the ACT-R folder in your preferred location path your-path.

- Download the INNER folder and put it under the actr7.x

- Launch ACT-R in the common lisp shell, like this:

code-block:: bash

    (load "..your-path.../act-r/actr7.x/load-act-r.lisp")
 
- Run the Python code under INNER folder, like this:

code-block:: bash

    >>>import demo
    >>>demo.start_trial()

Now you can see the model working in your ACT-R console.

You can hear the inner speech dialogue by installing the SpeechRecognition Python library at

 <https://pypi.org/project/SpeechRecognition/>

After SpeechRecognition installation, when you run the model you can hear the inner dialogue produced by your machine.

Running inner speech model on real robot
----------------------------------------


Operative systems:

- Ubuntu 16.04 LTS or latest

- Windows with Windows Subsystem for Linux (WSL) with Ubuntu 16.04 LTS or latest 
(you can follow the official instructions available at <https://docs.microsoft.com/en-us/windows/wsl/install-manual> for enabling WSL)

For integrating the framework on Pepper robot, just ROS kinetic version is required (that is the ROS version for MoveIt! library), which runs on Ubuntu 16.04 LTS.

If you will use a different robot model, please verify the suitable ROS version, and the corresponding Ubuntu version.

1 Prepare the environment
=========================

- Install ACT-R architecture

=============================

You have to install the latest version of ACT-R at

<http://act-r.psy.cmu.edu/software/>

Choose the standalone version of ACT-R

Choose the version according to your operative system.
You can install ACT-R in Windows if you are using WSL.

- Install MoveIt! (which requires ROS)

======================================

Follow the instructions available at 

http://docs.ros.org/kinetic/api/moveit_tutorials/html/doc/getting_started/getting_started.html

This is a complete guide which allows you to install the complete framework for running ROS and enabling robot movements during interaction.
Keep attention about ROS version

-  Install the configuration file of your robot

==============================================

For running Mo
You can find or built your own robot model by following the instuctions of the previous step in the section "Robot model and Robot state"

For Pepper, you can use the pepper_moveit_config package available at

<https://github.com/ros-naoqi/pepper_moveit_config>

Follow all the instructions to play with Pepper and try movements


2 Download ACT-R inner speech model
===================================

Put the content of the INNER folder under the actr7.x folder (that you find inside ACT-R distribution).

You can see the inner speech model by consulting the file inner_model.lisp
It contains all the knowledge and the rules implementing the inner speech ability for robot.

You can run the model without robot and you can see the inner dialogue in the prompt of ACT-R, or you can integrate that model on the robot by the steps described at point 3.

3 Compile








