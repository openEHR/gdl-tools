# gdl-tools

Guideline Definition Tools

## Getting Started

### Requirements

* Java 8 or higher
* Maven 3.0.4 or higher

### Usage

The java-libs project is available at [Maven Central](http://search.maven.org/).

For example, if you need to use the gdl-parser, add into your _pom.xml_:

```xml
<dependency>
    <groupId>org.gdl-lang.gdl-tools</groupId>
    <artifactId>gdl-parser</artifactId>
    <version>1.3.19</version>
</dependency>
```

### Installation

To build the whole project, first clone it, and once inside the project's folder (by default _gdl-tools_), run:
```bash
mvn clean install
```
This will create binary files in the _target_ directories of each submodule. The GDL editor binaries can be found in a zip file at _gdl-tools/cds/gdl-editor/target_.

### Binaries
A copy of the binaries for the gdl-editor can also be found at https://sourceforge.net/projects/gdl-editor
