<template>
  <div class="app-container">
    <el-row>
      <div>
      期望摊销其他费用 ：<span v-bind:style="{ color: textColor }">{{ statistics.totalOtherFee }}</span><br><br>
      当前摊销其他费用 ：<span v-bind:style="{ color: textColor1 }">{{ totalDevFee }}</span><br><br>
      </div>
    </el-row>
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="3">
        <el-button type="primary" icon="el-icon-plus" @click="handleClickEnable">拟定其他费用摊销</el-button>
      </el-col>
    </el-row>

    <el-table
      :data="otherAllList"
      style="width: 100%"
      ref="otherTable">
      <el-table-column prop="month" label="月度">
      </el-table-column>
      <el-table-column label="摊销费用" :sortable="false">
        <template slot-scope="scope">
          <el-input v-model="scope.row.devFee" placeholder="摊销费用" type="number" step="0.01" :disabled="!editable"/>
        </template>
      </el-table-column>
    </el-table>



<!--    &lt;!&ndash;-研发投入其他费用拟定;导出组件 &ndash;&gt;
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    &lt;!&ndash;-研发投入其他费用拟定;查看详情 &ndash;&gt;
    <budgetOtherFee-detail :show="budgetOtherFeeCard.show" v-model="budgetOtherFeeCard.key"
                    @handleClose="budgetOtherFeeClose"/>-->
    <br><br>
    <div style="display: flex; justify-content: center;">
    <el-button size="medium"  type="warning" @click="saveData">保存</el-button>
    </div>
  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import budgetOtherFeeDetail from '../common/budgetOtherFeeDetail'
  import {
    addBudgetOtherFee,
    delBudgetOtherFee,
    updateBudgetOtherFee,
    selectAll, updateByDev
  } from '@/api/cost/budgetOtherFee'
  import { isNullOrEmpty } from '@/utils/jq'
  import * as budgetOtherFeeApi from '@/api/cost/budgetOtherFee.js'


  export default {
    name: "budgetOtherFee",
    mixins: [JqTableMixin],
    provide() {
      return {
        handleQuery: this.handleQuery
      }
    },
    data() {
      return {
        textColor: 'green',
        textColor1: 'red',
        //查其他费用信息列表
        otherAllList: [],
        // 选中数组
        ids: [],
        // 非单个禁用
        single: true,
        // 非多个禁用
        multiple: true,
        // 显示搜索条件
        showSearch: true,
        // 是否显示数据
        exportTitle: "导出警告",
        // 是否显示导出弹出层
        exportOpen: false,
        // 查询参数
        queryParams: {},
        //表格配置数据
        tableConfig: {
          url: '/cost/budgetOtherFee/list',
          method: 'get',
          queryParams: null,
          orders: 'budget_other_fee.update_time desc',
          exportUrl: '/cost/budgetOtherFee/export',
          globalFlg: true,
          superSearch: {
            keyPlaceholder: '',
            radioSearch: true,
            radioData: []
          }
        },
        budgetOtherFeeCard: {
          show: false,
          key: null
        },
        statistics: {},
        editable: false,
      }
    },
    computed: {
      totalDevFee() {
        let total = 0;
        for (let i = 0; i < this.otherAllList.length; i++) {
          total += parseFloat(this.otherAllList[i].devFee);
        }
        return total.toFixed(2); // 将结果保留两位小数并返回
      }
    },
    components: {
      budgetOtherFeeDetail
    },
    watch:{
      '$store.state.item.itemNo'(newVal,oldVal){
        this.$nextTick(()=>{
          this.queryParams.itemNo = this.$store.state.item.itemNo;
          this.queryParams.companyId = this.$store.state.item.companyId;
            this.otherFee()
            this.budgetOtherFeeGetStatistics()
        })
      }
    },
    created() {
      this.otherFee()
      this.budgetOtherFeeGetStatistics()
      this.$nextTick(()=>{
        this.queryParams.itemNo = this.$store.state.item.itemNo;
        this.queryParams.companyId = this.$store.state.item.companyId;
        this.otherFee()
        this.budgetOtherFeeGetStatistics()
      })
    },
    methods: {
      otherFee() {
        selectAll(this.queryParams).then(res => {
          this.otherAllList = res.rows
          this.total = res.total
        })
      },
      /** 其他费用统计 */
      budgetOtherFeeGetStatistics() {
        if (!isNullOrEmpty(this.queryParams.companyId) && !isNullOrEmpty(this.queryParams.itemNo)) {
          budgetOtherFeeApi.getStatistics({
            companyId: this.queryParams.companyId,
            itemNo: this.queryParams.itemNo
          }).then(resp => {
            this.statistics = resp.data
          })
        }
      },
      saveData() {
              updateByDev(this.otherAllList).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                }
              });
      },
      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateBudgetOtherFee(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addBudgetOtherFee(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }

              });
            }
          }
        });
      },
      /** 搜索按钮操作 */
      handleQuery() {
        this.tableConfig.queryParams = this.queryParams
        this.getJqTableData()
        this. budgetOtherFeeGetStatistics()
      },

      /** 重置按钮操作 */
      resetQuery() {
        this.resetForm("queryForm");
        this.handleQuery();
        this. budgetOtherFeeGetStatistics()
      },

      /** 多选框选中数据 */
      handleSelectionChange(selection) {
        this.ids = selection.map(item => item.id)
        this.single = selection.length !== 1
        this.multiple = !selection.length
      },

      /** 新增按钮操作 */
      handleAdd() {
        this.budgetOtherFeeCard = {
          show: true,
          key: null
        }
      },

      /** 查看研发投入其他费用拟定; */
      budgetOtherFeeUpdate(row) {
        this.budgetOtherFeeCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭研发投入其他费用拟定;查看弹框 */
      budgetOtherFeeClose() {
        this.budgetOtherFeeCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        this.$confirm('是否确认删除研发投入其他费用拟定;编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delBudgetOtherFee(ids);
        }).then(() => {
          this.getJqTableData()
          this.msgSuccess("删除成功");
        }).catch(() => {
        })
      },

      /** 打开导出页面按钮 */
      confirmExport() {
        this.queryParams.exportType = 1;
        this.exportOpen = true;
      },

      /** 确认导出按钮操作 */
      handleExport() {
        if (0 === this.ids.length && 3 === this.queryParams.exportType) {
          this.msgError("未选中任何数据！");
          return
        }
        this.queryParams.ids = this.ids.join(",");
        let queryParams = this.queryParams;
        exportBudgetOtherFee(queryParams);
        this.exportOpen = false;
      },
      /** 修改操作 */
      handleClickEnable() {
        this.editable = true;
      }
    }
  }
</script>
