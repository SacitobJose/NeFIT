compileClient:
	./protos/protoc --java_out=. protos/protos.proto
	javac -cp protos/protobuf-java-3.6.1.jar client/Protos.java
	javac -cp protos/protobuf-java-3.6.1.jar client/*.java

runClient:
	java client.Client

runDealer:
	java dealer.Dealer