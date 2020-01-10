compileClient:
	./protos/protoc --java_out=. protos/protos.proto
	javac -cp protos/protobuf-java-3.6.1.jar protos/Protos.java
	javac -cp protos/protobuf-java-3.6.1.jar protos/*.java

runClient:
	java client.Client
