# Linux
compileProtos:
	./src/main/java/protos/protoc --java_out=src/main/java src/main/java/protos/protos.proto
	
	./src/main/java/protos/gpb/bin/protoc-erl -I. src/main/java/protos/protos.proto
	cp src/main/java/protos/protos.erl src/main/java/server
	cp src/main/java/protos/protos.hrl src/main/java/server
	cp src/main/java/protos/gpb/include/gpb.hrl src/main/java/server

# MacOS
compileProtos2:
	protoc --java_out=src/main/java src/main/java/protos/protos.proto
	src/main/java/protos/gpb/bin/protoc-erl -I. src/main/java/protos/protos.proto
	cp src/main/java/protos/protos.erl src/main/java/server
	cp src/main/java/protos/protos.hrl src/main/java/server
	cp src/main/java/protos/gpb/include/gpb.hrl src/main/java/server

compileServer:
	erlc -o src/main/java/server/ src/main/java/server/*.erl

compileJava:
	mvn compile

runClient:
	mvn exec:java -Dexec.mainClass="client.Client" -Dexec.args="${PID}"

runDealer:
	mvn exec:java -Dexec.mainClass="dealer.Dealer"

runCatalog:
	mvn exec:java -Dexec.mainClass="catalog.Catalog"
