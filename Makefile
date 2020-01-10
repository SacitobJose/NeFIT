compileProtos:
	./protos/protoc --java_out=. protos/protos.proto
	javac -cp protos/protobuf-java-3.6.1.jar protos/Protos.java

compileClient:
	javac -cp protos/protobuf-java-3.6.1.jar client/*.java

runClient:
	java client.Client

runDealer:
	java dealer.Dealer
