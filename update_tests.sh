#!/bin/bash
# Update all createSwitchbackApp calls to new Switchback
sed -i 's/createSwitchbackApp(/new Switchback(/g' src/index.test.ts

# The visit, page, reload functions are now instance methods
# We need to create an app instance in each test

