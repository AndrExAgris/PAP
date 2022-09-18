#include <stdio.h>
#include <stdbool.h>
 
int recsum(int n);
int findmin(int list[], int len);
int findmax(int list[], int len);
bool palin(char word[], int len);
int sumpos(int list[], int len);

int main()
{

    int x;
    printf("Digite:\n1 para achar o valor de uma soma \n2 menor elemento de vetor \n3 maior elemento de vetor \n4 verficar palindromo\n5 calcular soma positivos\n");
    scanf("%d", &x);
    /*
    não sei como o professor iria testar então fiz esse bando de if
    ia fazer um trycatch ou similar em c pra ficar melhor estruturado o codigo 
    mas fiz mais simples com ifs pra facilitar a vida
    */
    if(x==1){
        int res, n;
        printf("Digite um valor positivo\n");
        scanf("%d", &n);
        res = recsum(n);
        printf("%d\n", res);
    }
    if(x==2){
        int len, max;
        printf("Digite o tamanho do vetor\n");
        scanf("%d", &len);
        int list[len];
        int i;
        for(i=0;i<len;i++){
            scanf("%d", &list[i]);
        }
        /*
        não sei se o que fiz com vetores foi o que o professor queria
        mas ao menos acho que foi em recurção
        só que criar um vetor, na verdade dar valores pra ele é bem confuso pra mim
        e o unico jeito que pensei em fazer eu precusaria trocar variaveis todo "ciclo"
        então acabei usando o for
        */
        max = findmax(list, len-1);
        printf("%d\n", max);  
    }
    if(x==3){
        int len, min;
        printf("Digite o tamanho do vetor\n");
        scanf("%d", &len);
        int list[len];
        int i;
        for(i=0;i<len;i++){
            scanf("%d", &list[i]);
        }
        min = findmin(list, len-1);
        printf("%d\n", min);  
    }
    if(x==4){
        /*  
        tive bastante dificuldade tentando essa,
        entendi a ideia de comparar o primeiro valor com o ultimo e se são iguais continua
        mas isso em recurção ficou muito confuso pra mim
        */
    }
    if(x==5){
        int len, sum;
        printf("Digite o tamanho do vetor\n");
        scanf("%d", &len);
        int list[len];
        int i;
        for(i=0;i<len;i++){
            scanf("%d", &list[i]);
        }
        sum = sumpos(list, len-1);
        printf("%d\n", sum);  
    }
    return (0);
}

int recsum(int n)
{
    int variante;
    if(n<=0){
        return (0);
    }
    if(n>0){
        variante = n+ recsum(n-1);
        return(variante);
    }
}

int findmax(int list[], int len)
{
    if(len<=0){
        return(list[len]);
    }if(len>0){
        if(list[len]>list[len-1]){
            list[len-1] = list[len];
        }
        
        findmax(list, len-1);
    }
}

int findmin(int list[], int len)
{
    if(len<=0){
        return(list[len]);
    }if(len>0){
        if(list[len]<list[len-1]){
            list[len-1] = list[len];
        }
        
        findmin(list, len-1);
    }
}

bool palin(char word[], int len){
    return(true);
}

int sumpos(int list[], int len){
    if(len<=0){
        return(list[len]);
    }if(len>0){
        if(list[len]>0){
            if(list[len-1]>0){
                list[len-1]+=list[len];
            }else{
                list[len-1]=list[len];
            }
        }
        sumpos(list, len-1);
    }
}