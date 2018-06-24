#include <iostream>
#include <memory>
#include <typeinfo>

class AbstractRequest
{
public:
	AbstractRequest();
	~AbstractRequest();
public:
	virtual void BuildRequest() = 0;
	virtual void ProcessResponse() = 0;
	void Execute();
};

AbstractRequest::AbstractRequest()
{
	const std::type_info& typeInfo = typeid(this);
	std::cout << "ctor :" << typeInfo.name() << std::endl;
}

AbstractRequest::~AbstractRequest()
{
	const std::type_info& typeInfo = typeid(this);
	std::cout << "dtor :" << typeInfo.name() << std::endl;
}

void AbstractRequest::Execute()
{
	BuildRequest();
	ProcessResponse();
}

class RequestA :public AbstractRequest
{
public:
	void BuildRequest() override;
	void ProcessResponse() override;
};

void RequestA::BuildRequest()
{
	const std::type_info& typeInfo = typeid(this);
	std::cout << "Request " << typeInfo.name() << " implementation" << std::endl;
}

void RequestA::ProcessResponse()
{
	const std::type_info& typeInfo = typeid(this);
	std::cout << "Response " << typeInfo.name() << " implementation" << std::endl;
}

class RequestB :public AbstractRequest
{
public:
	void BuildRequest() override;
	void ProcessResponse() override;
};

void RequestB::BuildRequest()
{
	const std::type_info& typeInfo = typeid(this);
	std::cout << "Request " << typeInfo.name() << " implementation" << std::endl;
}

void RequestB::ProcessResponse()
{
	const type_info& typeInfo = typeid(this);
	std::cout << "Response " << typeInfo.name() << " implementation" << std::endl;
}

int main()
{
	/*
	std::unique_ptr<AbstractRequest> request(new RequestA());
	request->Execute();
	std::unique_ptr<AbstractRequest> request(new RequestB());
	request->Execute();
	*/

	AbstractRequest* request = new RequestA();
	request->Execute();
	delete request;

	request = new RequestB();
	request->Execute();
	delete request;
}
