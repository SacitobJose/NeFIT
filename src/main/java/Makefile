compileProtos:
	./protos/protoc --java_out=. protos/protos.proto
	javac -cp protos/protobuf-java-3.6.1.jar protos/Protos.java

	./protos/gpb/bin/protoc-erl -I. -maps protos/protos.proto
	cp protos/protos.erl server/protos.erl
	cp protos/protos.hrl server/protos.hrl

compileClient:
	javac -cp protos/protobuf-java-3.6.1.jar client/*.java

compileServer:
	./protos/gpb/bin/protoc-erl -I. -maps protos/protos.proto
	cp protos/protos.erl server/protos.erl
	cp protos/protos.hrl server/protos.hrl

runClient:
	mvn exec:java -Dexec.mainClass="client.Client"

runDealer:
	mvn exec:java -Dexec.mainClass="dealer.Dealer"
