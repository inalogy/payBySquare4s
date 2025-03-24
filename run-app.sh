#!/bin/bash

# Script to run the PayBySquare application and output only the base64 content

# Pass all command line arguments to the jar
java -Dapple.awt.UIElement=true -Djava.awt.headless=true -jar ./app/target/scala-2.13/payBySquareApp.jar "$@" 2>/dev/null