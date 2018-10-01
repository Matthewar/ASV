#!/usr/bin/python
import os
import re
import shutil
import subprocess
import sys

# Create webroot
WEBROOT = os.path.join(os.environ['HOME'],'gh_pages')
CWD = os.getcwd()
os.mkdir(WEBROOT)

# Copy readme page
shutil.copyfile(
        os.path.join(CWD,'README.md'),
        os.path.join(WEBROOT,'index.md')
        )

# Copy markdown documentation
MARKDOWN_DOCS_PATH = os.path.join(WEBROOT,'docs')
shutil.copytree(
        os.path.join(CWD,'docs'),
        os.path.join(WEBROOT,'docs')
        )

# Move Haddock documentation
HADDOCK_PATH = subprocess.check_output(["stack","path","--no-terminal","--local-doc-root"]).strip("\n\r\f")
shutil.copytree(
        HADDOCK_PATH,
        os.path.join(WEBROOT,'haddock')
        )

# Create Haddock documentation summary
os.mkdir(os.path.join(WEBROOT,'haddock-cov'))
with open(os.path.join(WEBROOT,'haddock-cov/Report.md'),'w') as haddockReport:
    haddockReport.write('# Haddock Coverage Report\n')
    haddockReport.write('```\n')

    buildHaddock = subprocess.Popen(
            ["stack","build","--no-terminal","--haddock"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
            )

    foundLog = False
    matchLogLine = re.compile(r' [0-9]{1,3}% \( +([0-9]+) / +([0-9]+)\)').match
    coveredPoints = 0
    coverPoints = 0
    for buildLogLine in iter(buildHaddock.stderr.readline,''):
        if foundLog:
            match = matchLogLine(buildLogLine)
            if match:
                haddockReport.write(buildLogLine[1:])
                coveredPoints += int(match.group(1))
                coverPoints += int(match.group(2))
            else:
                break
        else:
            if buildLogLine.startswith('Haddock coverage:'):
                foundLog = True
            else:
                pass

    haddockReport.write('```\n\n')
    haddockReport.write('# Calculated Coverage\n')

    if coverPoints != 0:
        haddockReport.write(str(100 * coveredPoints / coverPoints))
        haddockReport.write('%')
    else:
        haddockReport.write('Error calculating coverage, 0 points to cover found')
    haddockReport.write('\n')

# Create code coverage summary
os.mkdir(os.path.join(WEBROOT,'tests'))

with open(os.path.join(WEBROOT,'tests/Coverage.md'),'w') as basicCoverage:
    basicCoverage.write('# Basic Coverage Report\n')
    basicCoverage.write('```\n')

    coverageRun = subprocess.Popen(
            ["stack","test","--no-terminal","--coverage"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
            )

    foundLog = False
    matchLogLine = re.compile(r' +[0-9]{1,3}%').match
    for buildLogLine in iter(coverageRun.stderr.readline,''):
        if foundLog:
            if matchLogLine(buildLogLine):
                basicCoverage.write(buildLogLine[1:])
            else:
                break
        else:
            if buildLogLine.startswith('Generating coverage report for ASV\'s test-suite "ASV-test"'):
                foundLog = True
            else:
                pass

    basicCoverage.write('```\n')


# Create test list
with open(os.path.join(WEBROOT,'tests/List.md'),'w') as formattedTestList:
    testList = subprocess.Popen(
            ["stack","test","--no-terminal","--ta",'"--list-tests"'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
            )

    numberOfTests = 0
    testListDepth = 0

    formattedTestList.write('# Test List\n')

    for testLongName in iter(testList.stdout.readline,''):
        numberOfTests += 1
        extractedTestName = testLongName.strip('\r\f\n').split('.')
        newTestListDepth = len(extractedTestName)
        if newTestListDepth > testListDepth:
            for tmpDepth in xrange(testListDepth,newTestListDepth):
                formattedTestList.write('   ' * tmpDepth)
                formattedTestList.write('- ')
                formattedTestList.write(extractedTestName[tmpDepth])
                formattedTestList.write('\n')
        else:
            formattedTestList.write('   ' * newTestListDepth)
            formattedTestList.write('- ')
            formattedTestList.write(extractedTestName[-1])
            formattedTestList.write('\n')
        testListDepth = newTestListDepth

with open(os.path.join(WEBROOT,'tests/num-tests'),'w') as additionalTestData:
    additionalTestData.write(str(numberOfTests))
    additionalTestData.write('\n')

# Write relative path to web root to stdout
sys.stdout.write(os.path.relpath(WEBROOT,CWD))
sys.stdout.flush()
