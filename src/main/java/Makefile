compileProtos:
	./protos/protoc --java_out=. protos/protos.proto

compileServer:
	erlc server/*.erl

runClient:
	mvn exec:java -Dexec.mainClass="client.Client"

runDealer:
	mvn exec:java -Dexec.mainClass="dealer.Dealer"
