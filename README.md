# gdl-tools

Guideline Definition Tools

## Getting Started

### Requirements

* Java 8 or higher
* Maven 3.0.4 or higher
* OpenEHR java-libs (https://github.com/openEHR/java-libs)

### Installation

NOTE: gdl-tools has a dependency to openEHR java-libs, so you will need to install java-libs before running maven on this project. The java-libs version is defined in  _gdl-tools/pom.xml_, if you want to run with your compiled version of java-libs, you will need to change the _openehr-java-libs-version_ property to _0-SNAPSHOT_ (instead of the released version).

To build the whole project, clone it, and once inside the project's folder (by default _gdl-tools_), run:
```bash
mvn clean install
```
This will create binary files in the _target_ directories of each submodule. The GDL editor can be found at _gdl-tools/cds/gdl-editor/target_.

### Binaries
A copy of the binaries for the gdl-editor can be found at https://sourceforge.net/projects/gdl-editor
