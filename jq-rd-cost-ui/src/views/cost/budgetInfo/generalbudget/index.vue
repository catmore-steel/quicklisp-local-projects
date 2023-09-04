<template>
  <div class="workbench">
    <div class="bottom_box">
      <div class="left_box">
        <h4>序时帐分类占比
          <el-button type="primary" @click="handleSequential">
            序时帐归类
          </el-button>
        </h4>
        <div ref="sequentialChart" style="width: 100%; height: 85%;"></div>
      </div>

      <div class="left_box">
        <h4>初步拟定研发费用占比
          <el-button type="primary" @click="handleDevelopFee">
            研发费用拟定
          </el-button>
        </h4>
        <div ref="developFeeChart" style="width: 100%; height: 85%;"></div>
      </div>
    </div>
    <div class="bottom_box">
      <div class="right_box">
        <el-radio-group v-model="queryParams.accountType" @change="getList">
          <el-radio-button
            v-for="(item, index) in accountTypeOptions"
            :key="index"
            :label="item.dictValue"
          >
            {{ item.dictLabel }}
          </el-radio-button>
        </el-radio-group>
        <el-tooltip class="item" effect="dark" content="摘要/会计科目/科目编码/科目名称" placement="top">
          <el-input v-model="queryParams.keySearch" placeholder="请输入关键字" clearable
                    @clear="getList" @keyup.enter.native="getList" style="width: 300px;float: right"
          >
            <i slot="prefix" class="el-input__icon el-icon-search"></i>
          </el-input>
        </el-tooltip>
        <template>
          <div style="margin-top: 20px;">
            <el-table :data="list" :summary-method="getSummaries" show-summary>
              <el-table-column label="日期" prop="accDate" :sortable="true"/>
              <el-table-column label="摘要" prop="summary" :sortable="false"/>
              <el-table-column label="会记科目" prop="accountSubject" :sortable="false"/>
              <el-table-column label="科目编码" prop="subjectCode" :sortable="false"/>
              <el-table-column label="科目名称" prop="subjectName" :sortable="true"/>
              <el-table-column label="借方金额" prop="debitAmount" :sortable="false"/>
            </el-table>
            <pagination
              v-show="queryParams.total > 0"
              :total="queryParams.total"
              :page.sync="queryParams.pageNum"
              :limit.sync="queryParams.pageSize"
              @pagination="getList"
            />
          </div>
        </template>
      </div>
    </div>

    <!-- 序时帐归类 -->
    <ledger-classification :show.sync="sequentialOpen" @handleClose="handleChildrenClose"/>

    <!-- 研发费用拟定 -->
    <fee-proposed :show.sync="developFeeOpen" @handleClose="handleChildrenClose"/>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import feeProposed from '@/views/cost/budgetInfo/common/feeProposed.vue'
import ledgerClassification from '@/views/cost/budgetInfo/common/ledgerClassification.vue'
import { isNullOrEmpty } from '@/utils/jq'
import { getDevelopFeeChartByCompanyIdAndItemNo } from '@/api/cost/generalbudget'
import { getBaseAccountList, getSequentialChartByCompanyIdAndItemNo } from '@/api/cost/accountInfo'

export default {
  name: 'Generalbudget',
  mixins: [JqTableMixin],
  provide() {
    return {
      handleQuery: this.handleQuery
    }
  },
  data() {
    return {
      //序时账类型字典
      accountTypeOptions: [],
      //查询参数
      queryParams: {
        total: 0,
        pageNum: 1,
        pageSize: 15,
        orders: 'base_account_info.acc_date desc',
        accountType: '1'
      },
      //序时账类型数组
      list: [],

      //是否显示序时帐归类弹出层
      sequentialOpen: false,
      //是否显示费用拟定弹出层
      developFeeOpen: false
    }
  },
  components: {
    ledgerClassification,
    feeProposed
  },
  watch: {
    '$store.state.item.itemNo'(newVal, oldVal) {
      this.$nextTick(() => {
        this.initSequentialChart()
        this.initDevelopFeeChart()
        this.getList()
      })
    }
  },
  created() {
    //费用类型字典
    this.getDicts('account_type').then(response => {
      this.accountTypeOptions = response.data
    })
    this.handleQuery()
  },
  methods: {
    /** 序时账记录列表 */
    getList() {
      this.queryParams.companyId = this.$store.state.item.companyId
      this.queryParams.itemNo = this.$store.state.item.itemNo
      //默认查询人员人工费
      if (isNullOrEmpty(this.queryParams.accountType)) {
        this.queryParams.accountType = '1'
      }
      getBaseAccountList(this.queryParams).then(response => {
        this.list = response.rows
        this.queryParams.total = response.total
      })
    },

    handleQuery() {
      this.getList()
      this.initSequentialChart()
      this.initDevelopFeeChart()
    },

    /** 序时帐归类点击操作 */
    handleSequential() {
      this.sequentialOpen = true
    },

    /** 研发费用拟定点击操作 */
    handleDevelopFee() {
      this.developFeeOpen = true
    },

    /** 员工确认保存操作 */
    handleChildrenClose() {
      this.sequentialOpen = false
      this.developFeeOpen = false
    },

    /** 序时帐分类占比饼图 */
    initSequentialChart() {
      //等dom加载完毕在获取
      this.$nextTick(() => {
        let myChart = this.$echarts.init(this.$refs.sequentialChart)
        getSequentialChartByCompanyIdAndItemNo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(res => {
          myChart.setOption({
            title: {},
            tooltip: {
              trigger: 'item',
              formatter: '{a} <br/>{b} : {c} ({d}%)'
            },
            legend: {
              orient: 'vertical',
              top: 'bottom',
              left: 'right',
              textStyle: { //数值样式
                color: '#8c8c8c',
                fontSize: 12
              }
            },
            series: [
              {
                name: '序时账',
                type: 'pie',
                radius: '75%',
                center: ['50%', '50%'],
                avoidLabelOverlap: true,
                data: res.data,
                emphasis: {
                  itemStyle: {
                    shadowBlur: 10,
                    shadowOffsetX: 0,
                    shadowColor: 'rgba(0, 0, 0, 0.5)'
                  }
                },
                itemStyle: {
                  normal: {
                    label: {
                      show: true,
                      formatter: '{b} : {c} ({d}%)'
                    },
                    labelLine: {
                      show: true
                    },
                    color: function(colors) {
                      const colorList = [
                        '#fc8251',
                        '#5470c6',
                        '#91cd77',
                        '#ef6567',
                        '#f9c956',
                        '#75bedc',
                        '#dc038e'
                      ]
                      return colorList[colors.dataIndex]
                    }
                  }
                }
              }
            ]
          })
        })
      })
    },

    /** 初步拟定研发费用占比饼图 */
    initDevelopFeeChart() {
      //等dom加载完毕在获取
      this.$nextTick(() => {
        let myChart = this.$echarts.init(this.$refs.developFeeChart)
        getDevelopFeeChartByCompanyIdAndItemNo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(res => {
          {
            myChart.setOption({
              title: {},
              tooltip: {
                trigger: 'item',
                formatter: '{a} <br/>{b} : {c} ({d}%)'
              },
              legend: {
                orient: 'vertical',
                top: 'bottom',
                left: 'right',
                textStyle: { //数值样式
                  color: '#8c8c8c',
                  fontSize: 12
                }
              },
              series: [
                {
                  name: '专利数量',
                  type: 'pie',
                  radius: '75%',
                  center: ['50%', '50%'],
                  avoidLabelOverlap: true,
                  data: res.data,
                  emphasis: {
                    itemStyle: {
                      shadowBlur: 10,
                      shadowOffsetX: 0,
                      shadowColor: 'rgba(0, 0, 0, 0.5)'
                    }
                  },
                  itemStyle: {
                    normal: {
                      label: {
                        show: true,
                        formatter: '{b} : {c} ({d}%)'
                      },
                      labelLine: {
                        show: true
                      },
                      color: function(colors) {
                        const colorList = [
                          '#fc8251',
                          '#5470c6',
                          '#91cd77',
                          '#ef6567',
                          '#f9c956',
                          '#75bedc',
                          '#dc038e'
                        ]
                        return colorList[colors.dataIndex]
                      }
                    }
                  }
                }
              ]
            })
          }
        })
      })
    },

    /** 统计借方金额总价 */
    getSummaries(param) {
      const sums = []
      const { columns, data } = param
      let debitAmount = 0
      if (!isNullOrEmpty(this.list)) {
        this.list.forEach(e => {
          debitAmount += e.debitAmount
        })
      }
      columns.forEach((column, index) => {
        if (index === 0) {
          sums[index] = '总价'
          return
        }
        if (index === 5) {
          sums[index] = debitAmount
          return
        }
      })
      return sums
    }

  }
}
</script>

<style lang="scss" scoped>
.workbench {
  height: calc(100vh - 84px);
  background-color: #f4f4f4;
  padding: 10px;

  .box1 {
    border-radius: 6px;
    height: 20%;
    padding: 10px;
    background-color: #fff;

    .item_wrap {
      //margin-top: 2%;
      height: 80%;
      display: flex;
      justify-content: space-between;
      align-items: center;

      .item {
        width: 24%;
        height: 90%;
        cursor: pointer;
        border: 2px dashed #ec808d;
        display: flex;
        justify-content: space-between;
        padding: 10px 20px;

        .left_box {
          display: flex;
          flex-direction: column;
          justify-content: space-around;
          //align-items: center;
        }

        .title {
          font-size: 12px;
          color: #909399;
        }

        .num {
          font-size: 23px;
          color: #666;
          font-weight: 600;
        }

        img {
          width: 60px;
        }
      }

      .item:hover {
        border-radius: 5px;
      }
    }
  }

  .box2 {
    padding-top: 10px;
    height: 40%;

    .top_box {
      width: 100%;
      height: 100%;
      background-color: #fff;
      padding: 10px;
    }
  }

  .bottom_box {
    height: 48%;
    display: flex;
    padding-top: 10px;

    .left_box {
      width: calc(100% - 5px);
      background-color: #fff;
      padding: 10px 0 0 10px;
      margin-right: 10px;
    }

    .right_box {
      padding: 10px;
      border-radius: 6px;
      background-color: #fff;
      width: calc(100% - 5px);

      .grid_wrap {
        margin-top: 25px;
        display: grid;
        padding: 20px;
        height: calc(100% - 130px);
        grid-template-rows: repeat(2, 1fr);
        grid-template-columns: repeat(5, 1fr);
        grid-row-gap: 30px;
        grid-column-gap: 20px;

        .item {
          cursor: pointer;
          border: 2px dashed #f6ac4c;
          position: relative;
          text-align: center;

          .title {
            margin-top: 15px;
            font-size: 12px;
            color: #989898;
          }

          .num {
            margin-top: 10px;
            margin-bottom: 10px;
            color: #18adf0;
            font-size: 23px;
            font-weight: 600;
          }

          img {
            position: absolute;
            top: -25px;
            left: 50%;
            margin-left: -15px;
            width: 38px;
          }
        }

        .item:hover {
          border-radius: 5px;
        }
      }
    }

  }

  h4 {
    color: #666666;
    margin: 0 0 15px 0;
    border-left: 3px solid #0099ff;
    padding-left: 15px;
  }
}
</style>
