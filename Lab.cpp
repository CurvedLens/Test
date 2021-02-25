#include <iostream>
#include <cmath>
#include <vector>


using namespace std;

vector<vector<double> > operator + (vector<vector<double> > a,vector<vector<double> > b)
{
	int n=a.size();
	int n1=b.size();
	int w=a[0].size();
	int w1=b[0].size();
	vector<vector<double> > c(n,vector<double>(w,0));
	if(n==n1&&w==w1)
	{
	for(int i=0;i<n;i++)
	for(int j=0;j<w;j++)
	c[i][j]=a[i][j]+b[i][j];
    }
    else 
    cout<<"Additing is impossible"<<endl;
    return c;
}

vector<vector<double> > operator * (vector<vector<double> > a,vector<vector<double> > b)
{
	int n=a.size();
	int n1=b.size();
	int w=a[0].size();
	int w1=b[0].size();
	if(w==n1)
	{
	vector<vector<double> > c(n,vector<double>(w1,0));
	for(int i=0;i<n;i++)
	for(int j=0;j<w1;j++)
	for(int k=0;k<w;k++)
	c[i][j]+=a[i][k]*b[k][j];
	return c;
    }
    else
    {
    vector<vector<double> > c(n,vector<double>(w1,0));
    cout<<"Multiplying is impossible - "<<n<<" "<<n1<<" "<<w<<" "<<w1<<endl;
    return c;
	}    
}

vector<vector<double> > operator * (double a,vector<vector<double> > b)
{
	int n=b.size();
	int w=b[0].size();
	vector<vector<double> > c=b;
	for(int i=0;i<n;i++)
	for(int j=0;j<w;j++)
	c[i][j]*=a;
	return c;
}

void Show(vector<vector<double> > a)
{
	int n=a.size();
	int w=a[0].size();
	for(int i=0;i<n;i++)
	{
	for(int j=0;j<w;j++)
	{
	cout<<"["<<a[i][j]<<"] ";	
	}
	cout<<endl;
	}	
}

double Scal(vector<vector<double> > a,vector<vector<double> > b)
{
	int m=a[0].size();
	double Scal=0;
	if(m==b[0].size()&&m==1)
	{
		for(int i=0;i<a.size();i++)
		Scal+=a[i][0]*b[i][0];
	}
	return Scal;
}

double f(vector<vector<double> > a)
{
	if(a.size()==2)
		return 3*a[0][0]*a[0][0]+2*a[1][0]*a[1][0]-0.01*a[0][0]*a[1][0]+a[0][0]-a[1][0];
	else
	{
		cout<<"Mistake"<<endl;
		return 0;
	}	
}
double f1(vector<vector<double> > a)
{
	if(a.size()==2)
		return a[0][0]*a[0][0]+1000*a[1][0]*a[1][0]-2*a[0][0]*a[1][0]-10*a[0][0]+6004*a[1][0];
	else
	{
		cout<<"Mistake"<<endl;
		return 0;
	}	
}
double test (vector<vector<double> > a)
{
	if(a.size()==2)
		return (a[1][0]-a[0][0]*a[0][0])*(a[1][0]-a[0][0]*a[0][0])+100*(1-a[0][0])*(1-a[0][0]);
	else
	{
		cout<<"Mistake"<<endl;
		return 0;
	}	
}
double g(vector<vector<double> > a)
{
	if(a.size()==3)
		return a[0][0]+2*a[1][0]+3*a[2][0];
	else
	{
		cout<<"Mistake"<<endl;
		return 0;
	}
}

vector<vector<double> > df(double f(vector<vector<double> >),vector<vector<double> > x)
{
	double h=0.001;
	int n=x.size();
	vector<vector<double> > a(n,vector<double>(1,0));
	for(int i=0;i<n;i++)
	{
		x[i][0]+=h;
		a[i][0]=f(x)/(2.0*h);
		x[i][0]-=2.0*h;
		a[i][0]-=f(x)/(2.0*h);
		x[i][0]+=h;
	}
	return (-1)*a;
}

double Step (vector<vector<double> > a,vector<vector<double> > b,vector<vector<double> > der,vector<vector<double> > x)
{
	double t=-1.0*Scal(a*x+b,der)/Scal(a*der,der);
	cout<<"\t Step\t"<<t<<endl;
	return t;
}

double StepD (double f(vector<vector<double> >), vector<vector<double> > x,vector<vector<double> > h,int  o)
{
	double lan=0.5,al=0.5;
	int i=1;
	while(f(x+al*h)>f(x))
	{	
		//cout<<"Iter is "<<i<<"\t Value is "<<al<<"\t New "<<f(x+al*h)<<"\t Old "<<f(x)<<endl;
		al*=lan;
		i++;
	}
		if(o>=0)
		cout<<"Iter is "<<i<<"\t Value is "<<al<<"\t New "<<f(x+al*h)<<"\t Old "<<f(x)<<endl;
		//al*=lan;
	return al;
}
vector<vector<double> > Grad(vector<vector<double> > a,vector<vector<double> > b, vector<vector<double> > start,double f(vector<vector<double> > ), int o)
{   
	//cout<<"This is gradient method.\n Input accuracy - "<<endl;
	double eps=0.0001;
	double k=1,z=1;
	int j=0;
	vector<vector<double> > temp(start.size(),vector<double>(1,-1));
	cout<<"Starting position"<<endl;
	Show(start);
	
	while(  sqrt(k)>eps or z>eps)
	{
		j++;
		cout<<j<<". ";
		temp=start;
		//start=temp+(Step(a,b,df(f,temp),temp))*df(f,temp);
		start=temp+StepD(f,temp,df(f,temp),o)*df(f,temp);
		k=Scal(temp+(-1)*start,temp+(-1)*start);
		z=fabs(f(temp)-f(start));
		if(o<=0)
		{
		cout<<"Value "<<f(start)<<endl;
		cout<<"Found points "<<endl;
		Show(start);
		}
	}
	cout<<"Iteration is "<<j<<" Answer"<<endl;
	return start;
}
vector<vector<double> > GradF(vector<vector<double> > a,vector<vector<double> > b, vector<vector<double> > start,double f(vector<vector<double> > ), int o)
{   
	//cout<<"This is gradient method.\n Input accuracy - "<<endl;
	double eps=0.0001;
	double k=1,z=1;
	int j=0;
//	while(eps<=0)
//	cin>>eps;
	vector<vector<double> > temp(start.size(),vector<double>(1,-1));
	cout<<"Starting position"<<endl;
	Show(start);
	
	while( sqrt(k)>eps or z>eps)
	{
		j++;
		cout<<j<<". ";
		temp=start;
		start=temp+(Step(a,b,df(f,temp),temp))*df(f,temp);
		//start=temp+StepD(f,temp,df(f,temp),o)*df(f,temp);
		k=Scal(temp+(-1)*start,temp+(-1)*start);
		z=fabs(f(temp)-f(start));
		if(o<=0)
		{
		cout<<"Value "<<f(start)<<endl;
		cout<<"Found points "<<endl;
		Show(start);
		}
	}
	cout<<"Iteration is "<<j<<" Answer"<<endl;
	return start;
}

vector<vector<double> > Newton(vector<vector<double> > a,vector<vector<double> > x,double f(vector<vector<double> >))
{
	
	int n=a.size();
	vector<vector<double> > temp(n,vector<double>(1,1));
	vector<vector<double> > obr=a;
	double eps=0.0001;
	if(n=a[0].size() and 2==n)
	{
		cout<<"Simplified condition!"<<endl;
		double t=obr[0][0];
		obr[0][0]=obr[1][1];
		obr[1][1]=t;
		obr[0][1]=obr[1][0]=-1.0*obr[0][1];
		obr=(1.0/(obr[0][0]*obr[1][1]-obr[0][1]*obr[0][1]))*obr;
		while(Scal(temp+(-1)*x,temp+(-1)*x)>eps)
		{
			temp=x;
			x=temp+obr*df(f,temp);
//			cout<<"Old x"<<endl;
//			Show(temp);
//			cout<<"New x"<<endl;
//			Show(x);
			cout<<"Value is "<<f(x)<<endl;
		}
		cout<<"Answer"<<endl;
		return x;
	}
	else
	{
		cout<<"Not simple"<<endl;
		return a;
	}
}

vector<vector<double> > Pm(vector<vector<double> > a,vector<vector<double> > x,double r)
{
	return x+(r/(sqrt(Scal(x+(-1)*a,x+(-1)*a))))*(a+(-1.0)*x);
}

vector<vector<double> > PmGrad(vector<vector<double> > x, vector<vector<double> > start,double g(vector<vector<double> >))
{
	double eps=0.000001;
	vector<vector<double> > temp(start.size(),vector<double>(1,2));
    
	while(Scal(temp+(-1)*start,temp+(-1)*start)>eps)
	{
		temp=start;
		start=temp+0.1*df(g,temp);
		start=Pm(start,x,1);
		Show(start);
		cout<<"Value "<<g(start)<<endl;
	}
	return start;
}

vector<vector<double> > hn(vector<vector<double> > x,vector<vector<double> > A,vector<vector<double> > h,double f(vector<vector<double> >))
{
	return df(f,x)+Scal((-1.0)*df(f,x),A*h)/Scal(h,A*h)*h;
}
vector<vector<double> > BetterGrad(vector<vector<double> > a,vector<vector<double> > b, vector<vector<double> > start,double f(vector<vector<double> > ))
{
	double eps=0.0001;
	int i=0;
	vector<vector<double> > temp(start.size(),vector<double>(1,1));
	vector<vector<double> > oh=df(f,start);
	while(Scal(temp+(-1)*start,temp+(-1)*start)>eps and i<4)
	{
		i++;
		cout<<"i="<<i<<endl;
		temp=start;
		start=temp+(Step(a,b,oh,temp))*oh;
		oh=hn(start,a,oh,f);
		//Show(start);
		cout<<"Value "<<f(start)<<endl;
	}
	cout<<"Answer"<<endl;
	return start;
}

int main()
{
	vector<vector<double> > a(2,vector<double>(2,-0.01));
	vector<vector<double> > b(2,vector<double>(1,1));
	a[0][0]=6;
	a[1][1]=4;
	b[1][0]=-1;
	
	vector<vector<double> > Sm(2,vector<double>(1,1));
	vector<vector<double> > Big(2,vector<double>(1,10));
	vector<vector<double> > La(2,vector<double>(1,100));
	vector<vector<double> > En(2,vector<double>(1,100000));
	vector<vector<double> > d(2,vector<double>(1,1));
	cout<<"Grad"<<endl;
	//Show(Grad(a,b,c,test,1));

	vector<vector<double> > a1(2,vector<double>(2,-2));
	vector<vector<double> > b1(2,vector<double>(1,-10));
	a1[0][0]=2;
	a1[1][1]=2000;
	b1[1][0]=6004;
	//Show(GradF(a1,b1,Big,f1,0));
	Show(Grad(a1,b1,La,f1,1));
	
	Show(Grad(a,b,Sm,f,1));
	//Show(GradF(a,b,Sm,f,1));
	Show(Grad(a,b,Big,f,1));
	//Show(GradF(a,b,Big,f,1));
	Show(Grad(a,b,La,f,1));
	//Show(GradF(a,b,La,f,1));
	Show(Grad(a,b,En,f,1));
	//Show(GradF(a,b,En,f,1));
	return 1;
	
}

	
	//cout<<"Newton"<<endl;
	//Show(Newton(a,c,f));
	//cout<<"BiGrad"<<endl;
	//Show(BetterGrad(a,b,c,f));
