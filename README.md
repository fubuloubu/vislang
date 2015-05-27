# vislang: a visual programming language

The point of vislang is to create a non-proprietary language used to develop software for embedded processors that is easily extensible via user-generated libraries, adaptable to many different platforms (currently supporting arduino), machine readable (using standard XML in order to work well with any additinal user-generated content created for visual editors), and fast (will compile directly to binary).

Current languages that provide a similar environment like Simulink, modelica, etc. are usually proprietary, complex (containing both code and visual representation definitions for a wide range of applications), and only translate from source to an intermediary language. Our choice of XML for our programming language allows maximum flexibility for use in 3rd party editors leveraging GUIs and ease of parsing for our compiler.
