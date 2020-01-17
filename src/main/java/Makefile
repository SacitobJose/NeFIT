compile:
	mvn compile

compileProtos:
	./protos/protoc --java_out=. protos/protos.proto

	./protos/gpb/bin/protoc-erl -I. -maps protos/protos.proto
	cp protos/protos.erl server/protos.erl
	cp protos/protos.hrl server/protos.hrl

compileServer:
	erlc server/*.erl

runClient:
	mvn exec:java -Dexec.mainClass="client.Client"

runDealer:
	mvn exec:java -Dexec.mainClass="dealer.Dealer"
