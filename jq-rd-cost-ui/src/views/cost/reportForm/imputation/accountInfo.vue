<template>
  <div class="app-container">
    <el-row style="margin: 5px 0px;">
    <el-col :span="4">
      <el-input v-model="keySearch" placeholder="请输入关键字" @input="costAccountInfoList">
              <i slot="prefix" class="el-input__icon el-icon-search"></i>
      </el-input>
    </el-col>
    <el-col :span="6" style="padding: 2px;">
      <span class="u10685">未分配金额{{statistics.distributableAmountTotal}}元,</span>
      <span class="u10685">分配金额{{statistics.allocatedAmountTotal}}元</span>
    </el-col>
    </el-row>
    <el-form ref="detail" :model="detail" :rules="detail.rules"  label-width="100px">
      <el-table :data="detail.tableData" style="margin-bottom: 22px;">
        <el-table-column label="原始序时账编号" prop="accountNo">
          </el-table-column>
          <el-table-column label="日期" prop="accDate">
          </el-table-column>
          <el-table-column label="凭证号" prop="credentialsNo">
          </el-table-column>
          <el-table-column label="摘要" prop="summary">
          </el-table-column>
          <el-table-column label="会计科目" prop="accountSubject">
          </el-table-column>
          <el-table-column label="借款金额" prop="debitAmount">
          </el-table-column>
          <el-table-column label="已分配金额" prop="allocatedAmount">
          </el-table-column>
          <el-table-column label="可分配金额" prop="distributableAmount">
          </el-table-column>
          <el-table-column label="分配金额" prop="allocationAmount">
            <template slot-scope="scope">
              <el-form-item :prop="'tableData.' + scope.$index + '.allocationAmount'" :rules="detail.rules.allocationAmount">
                <el-input v-model.number="scope.row.allocationAmount" @input="val => scope.row.allocationAmount = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')"></el-input>
              </el-form-item>
            </template>
          </el-table-column>
        </el-table>
        <pagination :total="total" :limit.sync="pageSize" @pagination="handPagination" />
        <div style="display: flex;justify-content:center">
          <el-button type="primary"  @click="submitForm" :loading="loading">确 定</el-button>
          <el-button @click="handleCancel">取 消</el-button>
        </div>
    </el-form>                   
  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import { delAccountInfo, exportAccountInfo } from '@/api/cost/accountInfo'
  import * as apiFeeCollectInfo from '@/api/cost/feeCollectInfo'

  
  export default {
    name: "accountInfo",
    mixins: [JqTableMixin],
    provide() {
      return {
        handleQuery: this.handleQuery
      }
    },
    props:{
      row:{}
    },
    data() {
      return {
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
        //导入弹出层title
        importTitle: "导入",
        // 是否显示导出弹出层
        importOpen: false,
        url: "",
        // 查询参数
        queryParams: {},
        accountInfoCard: {
          show: false,
          key: null
        },
        itemNo: this.$store.state.item.itemNo,
        companyId: this.$store.state.item.companyId,
        detail: {
          tableData: [],
          rules: {
            allocationAmount: [{
              required: true, validator: (rule, value, callback) => {
                console.log('validator',rule, value, callback);
                let idx = rule.fullField.split(".")[1];
                let record = this.detail.tableData[idx]
                if (record.allocationAmount&&(record.allocationAmount>record.distributableAmount)) {
                  this.$message.error('分配金额:' + record.allocationAmount + ',不能大于可分配金额:' + record.distributableAmount)
                  return callback(new Error('err'));
                }
                return callback();
              }, trigger: 'change'
            }]
          }
        },
        pageNum: 1,
        pageSize: 15,
        total: 0,
        loading:false,
        keySearch:'',
        statistics:{}
      }
    },
    components: {
      
    },
    created() {
      this.costAccountInfoList();
    },
    methods: {

      costAccountInfoList() {
        apiFeeCollectInfo.costAccountInfoList({
          itemNo:this.itemNo,
          companyId:this.companyId,
          pageNum:this.pageNum,
          pageSize: this.pageSize,
          keySearch:this.keySearch
        }).then(resp=>{
          this.detail.tableData = resp.data.tableBodyList;
          this.total = resp.data.total
        })

        apiFeeCollectInfo.costAccountInfoStatistics({
          itemNo:this.itemNo,
          companyId:this.companyId,
          keySearch:this.keySearch
        }).then(resp=>{
          this.statistics = resp.data
        })

      },

      handPagination(val) {
        this.pageNum = val.page
        this.pageSize = val.limit
        this.costAccountInfoList()
      },
      /** 新增按钮操作 */
      handleCancel() {
        console.log('handleCancel')
        this.$emit('update:projectOpen', false)
        //$emit('update:show', false
      },

      /** 查看原始序时账信息 */
      accountInfoUpdate(row) {
        this.accountInfoCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
      },

      /** 关闭原始序时账信息查看弹框 */
      accountInfoClose() {
        this.accountInfoCard = {
          show: false,
          row: null
        }
      },
      submitForm() {
        //console.log("JqTableRef",this.$refs['JqTableRef'].tableData)
        this.$refs['detail'].validate(valid => {
          console.log("valid",valid)
          //return;
          if (valid) {
            if (true) {
              let feeCollectDetails = []
              
              this.detail.tableData.filter(elt=>Number(elt.allocationAmount)>0)
              .forEach(elt=>{
                feeCollectDetails.push({
                  money:elt.allocationAmount,
                  accountNo:elt.accountNo,
                  delFlag:'0'
                })
              });
              this.loading=true;
              apiFeeCollectInfo.saveForReqVO({feeCollectDetails:feeCollectDetails,
                feeCollectInfo:{
                  projectId: this.row.projectId,
                  feeItem: this.row.feeItem,
                  month: this.row.month,
                  deptType: this.row.deptType,
                  itemNo: this.itemNo,
                  companyId: this.companyId,
                  delFlag: '0',
                }})
              .then(resp=>{
                this.loading=false;
                this.$message({message:resp.msg})
                this.$emit('update:projectOpen', false)
                this.$emit('handleQuery');
              })
            
            }
          }
        })
      }
    }
  }
</script>
<style lang="scss" scoped>
.u10685 {
  margin-left: 10px;
  font-family:'PingFangSC-Semibold', 'PingFang SC Semibold', 'PingFang SC', sans-serif;
  font-weight:650;
  font-style:normal;
  font-size:12px;
  color:#000000;
  text-align:center;
  line-height:18px;
}
</style>