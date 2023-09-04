<template>
  <div class="app-container">
    <el-row>
      <div>
        期望摊销其他费用 ：<span v-bind:style="{ color: 'red' }">{{ statistics.totalDesignFee }}</span><br><br>
        当前摊销其他费用 ：<span v-bind:style="{ color: 'green' }">{{ totalDesFee }}</span><br><br>
      </div>
    </el-row>
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" @click="handleClickEnable">拟定新品设计摊销</el-button>
      </el-col>
    </el-row>

    <el-table
      :data="designFeeList"
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

    <br><br>
    <div style="display: flex; justify-content: center;">
      <el-button size="medium"  type="warning" @click="saveData">保存</el-button>
    </div>
  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import budgetDesignFeeDetail from '../common/budgetDesignFeeDetail'
  import { delBudgetDesignFee, selectDes, updateByDes } from '@/api/cost/budgetDesignFee'
  import { selectAll, updateByDev } from '@/api/cost/budgetOtherFee'
  import { isNullOrEmpty } from '@/utils/jq'
  import { getStatistics } from '@/api/cost/budgetDeviceFee'
  import * as budgetDesignFeeApi from '@/api/cost/budgetDesignFee.js'

  export default {
    name: "budgetDesignFee",
    mixins: [JqTableMixin],
    provide() {
      return {
        handleQuery: this.handleQuery
      }
    },
    data() {
      return {
        //新品设计列表
        designFeeList: [],
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
          url: '/cost/budgetDesignFee/list',
          method: 'get',
          queryParams: null,
          orders: 'budget_design_fee.update_time desc',
          exportUrl: '/cost/budgetDesignFee/export',
          superSearch: {
            keyPlaceholder: '',
            radioSearch: true,
            radioData: [{label: '搜索一', value: 1}, {label: '搜索二', value: 2}]
          }
        },
        budgetDesignFeeCard: {
          show: false,
          key: null
        },
        statistics: {},
        editable: false,
      }
    },
    computed: {
      totalDesFee() {
        let total = 0;
        for (let i = 0; i < this.designFeeList.length; i++) {
          total += parseFloat(this.designFeeList[i].devFee);
        }
        return total.toFixed(2); // 将结果保留两位小数并返回
      }
    },
    components: {
      budgetDesignFeeDetail
    },
    watch:{
      '$store.state.item.itemNo'(newVal,oldVal){
        this.$nextTick(()=>{
          this.queryParams.itemNo = this.$store.state.item.itemNo;
          this.queryParams.companyId = this.$store.state.item.companyId;
          this.designFee()
          this.budgetDesignFeeGetStatistics()
        })
      }
    },
    created() {
      this.designFee()
      this.budgetDesignFeeGetStatistics()
      this.$nextTick(()=>{
          this.queryParams.itemNo = this.$store.state.item.itemNo;
          this.queryParams.companyId = this.$store.state.item.companyId;
          this.designFee()
          this.budgetDesignFeeGetStatistics()
      })
    },
    methods: {
      designFee() {
        selectDes(this.queryParams).then(res => {
          this.designFeeList = res.rows
          this.total = res.total
        })
      },
      /** 设计费用统计 */
      budgetDesignFeeGetStatistics() {
        if (!isNullOrEmpty(this.queryParams.companyId) && !isNullOrEmpty(this.queryParams.itemNo)) {
          budgetDesignFeeApi.getStatistics({
            companyId: this.queryParams.companyId,
            itemNo: this.queryParams.itemNo
          }).then(resp => {
            this.statistics = resp.data
          })
        }
      },
      saveData() {
        updateByDes(this.designFeeList).then(response => {
          if (response.code === 200) {
            this.msgSuccess(response.msg)
          }
        });
      },

      /** 搜索按钮操作 */
      handleQuery() {
        this.tableConfig.queryParams = this.queryParams
        this.getJqTableData()
        this.budgetDesignFeeGetStatistics()
      },

      /** 重置按钮操作 */
      resetQuery() {
        this.resetForm("queryForm");
        this.handleQuery();
        this.budgetDesignFeeGetStatistics()
      },

      /** 多选框选中数据 */
      handleSelectionChange(selection) {
        this.ids = selection.map(item => item.id)
        this.single = selection.length !== 1
        this.multiple = !selection.length
      },

      /** 新增按钮操作 */
      handleAdd() {
        this.budgetDesignFeeCard = {
          show: true,
          key: null
        }
      },

      /** 查看研发投入新品设计拟定 */
      budgetDesignFeeUpdate(row) {
        this.budgetDesignFeeCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭研发投入新品设计拟定查看弹框 */
      budgetDesignFeeClose() {
        this.budgetDesignFeeCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        this.$confirm('是否确认删除研发投入新品设计拟定编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delBudgetDesignFee(ids);
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
        exportBudgetDesignFee(queryParams);
        this.exportOpen = false;
      },
      /** 修改操作 */
      handleClickEnable() {
        this.editable = true;
      }
    }
  }
</script>
