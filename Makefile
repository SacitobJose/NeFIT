compileProtos:
	./src/main/java/protos/protoc --java_out=src/main/java src/main/java/protos/protos.proto

compileServer:
	erlc -o src/main/java/server/ src/main/java/server/*.erl

compileJava:
	mvn compile

runClient:
	mvn exec:java -Dexec.mainClass="client.Client"

runDealer:
	mvn exec:java -Dexec.mainClass="dealer.Dealer"
