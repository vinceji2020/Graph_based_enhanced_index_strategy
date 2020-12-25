# Graph_based_enhanced_index_strategy

传统金融模型中通常运用相关性和协方差矩阵等模型来对股票间相互作用的关系进行建模。但是现有的模型被发现对估计误差极其敏感，虽然各类研究提出了各种调整的方法使预测值尽量逼近真实值，一些常见的应用，如均值方差组合优化，在实际操作中还是会有样本内表现不俗、样本外表现差强人意的情况。本策略的主要目的是从传统金融建模的线性思维中更进一步，利用工程学的网络模型与金融系统的相似性进行非线性高维度的建模。通过非线性高维度的模型获得信息优势，从而更好的构建投资组合并获得更高的收益。

利用图结构G(V,E)里的节点V来表示每一个在股票池里的股票，用边E来表示股票之间的关系。对于每条边的长度，我们用基于互信息度量I_S (X,Y)的互信息距离d(X,Y)=1-√(1-exp⁡(-2I_S (X,Y)))来衡量。通过计算每对节点的互信息度距离，构造图结构(graph)的邻接矩阵。我们可以通过对互信息距离设定一个阈值来对完全连接的图结构进行初步剪枝，从而得到一个复杂度略低的高度连接网络。接下来我们更进一步运用最小生成树算法对网络进行进一步化简，在保证系统基本结构的情况下、剔除尽可能多的冗余信息。最后，我们基于最小生成树计算每个节点的中心性，并用中心性构建投资组合。

基于网络结构模型的指数增强策略：选用沪深300作为基准，使用网络结构的紧密中心性、度中心性和介数中心性分别选取沪深300成分股中心性前20%的“中心资产”构建组合。我们选用的三种中心性构建的组合相对于基准沪深300的表现都有所增强。其中介数中心性在A股市场表现最好，自2013年7月至2020年7月，获得了187.96%的累计收益，16.31%的年化收益，相对于基准沪深300获得了5.05%的年化超额收益和53.57%的月度胜率
