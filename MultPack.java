/**
 * @program: helloworld
 * @author: wqdong
 * @description: review of gmcm2015 problem D
 * @create: 2018-08-15 08:05
 **/
public class MultPack {
    public  static  int MAX = 10000;
    public  static int[][] f= new int [MAX][MAX]; // 存储最优解
    public static void main(String[] args){
        int pack = 6390;
        int n = 41;

        int [] value ={27,27,26,25,24,23,22,20,18,16,
                15,15,15,15,15,16,17,18,19,20,
                21,23,24,25,25,26,28,30,32,34,
                36,37,37,37,38,39,40,40,40,40,40};

        double[] weight = new double[n];
        int [] account = new int[n];

        for(int i=0;i<weight.length;i++){
            weight[i] = ((double) i+(double) 620)/(double)10;
            account[i] = 55;
        }

        for(int i=0;i<weight.length;i++){
            System.out.print(weight[i] +" ");
        }

        System.out.println(" ");

        int max = MultPackV(value, weight, account, n, pack);

        System.out.println("执行完毕");
    }

    public static int MultPackV(int v[], double w[], int t[], int n, int V)
    {
        // 初始化
        for (int j = 0; j <= V; j++)
            f[0][j] = 0;

        // 求解最优解
        for (int i = 1; i <= n; i++)
        {
            for (int j = (int)w[i - 1]; j <= V; j++)
            {
                // 第 i 个物品的最大可能数量
                int count = Math.min(t[i - 1], j / (int)w[i - 1]);
                int tmp = 0;
                for (int k = 0; k <= count; k++)
                {
                    if (f[i - 1][j - (int)w[i - 1] * k] + v[i - 1] * k > tmp)
                        tmp = f[i - 1][j - (int)w[i - 1]*k] + v[i - 1] * k;
                }
                f[i][j] = Math.max(f[i - 1][j], tmp);
            }
        }

        int c = V;
        int[] x = new int [n]; // 分配状态
        for (int i = n; i >= 1; i--)
        {
            for (int j = 0; j <= V / w[i - 1] && j <= t[i -1]; j++)
            {
                if (f[i][c] == f[i - 1][c - j * (int)w[i - 1]] + j * v[i - 1])
                {
                    x[i - 1] = j;
                    c -= j * w[i-1];
                    break;
                }
                else
                {
                    x[i - 1] = 0;
                }
            }
        }

        // 打印最终结果
        System.out.println(" 最大价值为："+f[n][V]);
        System.out.println(" 分配状态为：");
        for(int i = 0;i <n;i++)
        {
            System.out.print(x[i]+" ");
        }
        System.out.println();
        double rw = 0;
        double rv = 0;
        for(int i =0;i<n;i++){
            if(x[i]!=0){
                rw += (double) x[i]* w[i];
                rv += (double) x[i]* v[i];
            }
        }
        System.out.print(" 背包空间= "+V+" 物品体积= "+rw+" 物品价值= "+rv);
        return f[n][V];
    }


}
