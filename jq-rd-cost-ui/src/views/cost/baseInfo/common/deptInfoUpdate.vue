<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <jq-panel title="基本信息">
      <el-row>
        <el-col :span="8">
          <el-form-item label="上级部门" prop="parentId">
            <treeselect v-model="detail.parentId" :options="deptInfos" :disabled="disabled" :show-count="true"     class="form-control"  placeholder="请选择归属部门,空则为根机构"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="部门名称" prop="deptName">
            <el-input v-model="detail.deptName" placeholder="请输入部门名称" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="部门类型" prop="deptType">
            <el-select v-model="detail.deptType" placeholder="请选择部门类型" class="form-control" clearable filterable>
              <el-option
                v-for="dict in deptTypeOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
      </el-row>
    </jq-panel>
    <jq-panel title="序时帐科目">
      <el-row>
        <el-col :span="8">
          <el-form-item label="工资" prop="payWages">
            <el-input v-model="detail.payWages" placeholder="请输入工资" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="奖金" prop="payBonus">
            <el-input v-model="detail.payBonus" placeholder="请输入奖金" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="其他工资" prop="payOtherWages">
            <el-input v-model="detail.payOtherWages" placeholder="请输入其他工资" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="保险" prop="payInsure">
            <el-input v-model="detail.payInsure" placeholder="请输入保险" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="公积金" prop="payAccfund">
            <el-input v-model="detail.payAccfund" placeholder="请输入公积金" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="其他保险" prop="payOtherInsure">
            <el-input v-model="detail.payOtherInsure" placeholder="请输入其他保险" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="折旧" prop="payDepreciation">
            <el-input v-model="detail.payDepreciation" placeholder="请输入折旧" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="无形资产" prop="payAssets">
            <el-input v-model="detail.payAssets" placeholder="请输入无形资产" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="材料科目" prop="paySubject">
            <el-input v-model="detail.paySubject" placeholder="请输入材料科目" />
          </el-form-item>
        </el-col>
      </el-row>
    </jq-panel>
    <jq-panel title="费用项目">
      <el-row>
        <el-col :span="8">
          <el-form-item label="工资" prop="feeWages">
            <el-input v-model="detail.feeWages" placeholder="请输入工资" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="保险" prop="feeInsure">
            <el-input v-model="detail.feeInsure" placeholder="请输入保险" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="奖金" prop="feeBonus">
            <el-input v-model="detail.feeBonus" placeholder="请输入奖金" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="公积金" prop="feeAccfund">
            <el-input v-model="detail.feeAccfund" placeholder="请输入公积金" />
          </el-form-item>
        </el-col>
        <el-col :span="16">
        </el-col>
      </el-row>
      </jq-panel>
      <div class="fixed_coperate" v-show="!disabled">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </el-form>
  </div>
</template>

<script>
  import {isNullOrEmpty} from '@/utils/jq'
  import {getDeptInfo, delDeptInfo, addDeptInfo, updateDeptInfo,costDeptInfoTreeselect} from "@/api/cost/deptInfo";


  export default {
    name: 'deptInfoUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        deptTypeOptions: [],
        detail: {},
        //表单校验
        rules: {
          deptName: [
            { required: true, message: "部门名称不能为空", trigger: "blur" }
          ],
          deptType: [
            { required: true, message: "部门类型不能为空", trigger: "blur" }
          ]
        },
        companyId: this.$store.state.item.companyId,
        itemNo: this.$store.state.item.itemNo,
        deptInfos:[]
      }
    },
    props: {
      deptInfoId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      },
      deptInfoPid: {
        type: String
      }
    },
    watch: {
      deptInfoId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      },
      deptInfoPid(val){
        this.detail.parentId=val
      }
    },
    created() {
      this.getDicts('dept_type').then(response => {
        this.deptTypeOptions = response.data
      })
      this.ApiCostDeptInfoTreeselect();
      this.getDetail(this.deptInfoId)
      if (this.deptInfoPid) {
        this.detail.parentId=this.deptInfoPid
      }

    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          parentId: null,
          deptName: null,
          deptType: null,
          payWages: null,
          payBonus: null,
          payOtherWages: null,
          payInsure: null,
          payAccfund: null,
          payOtherInsure: null,
          payDepreciation: null,
          payAssets: null,
          paySubject: null,
          feeWages: null,
          feeInsure: null,
          feeBonus: null,
          feeAccfund: null,
          itemNo: null,
          companyId: null,
          tenantId: null,
          revision: null,
          createBy: null,
          createTime: null,
          updateBy: null,
          updateTime: null,
          remark: null,
          delFlag: null
        }
        this.resetForm("detail");
      },

      /** 获取企业部门信息详情 */
      getDetail(deptInfoId) {
        if (isNullOrEmpty(deptInfoId)) {
          this.reset()
        } else {
          getDeptInfo(deptInfoId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            this.detail.companyId=this.companyId;
            this.detail.itemNo=this.itemNo;
            this.detail.delFlag = 0;
            if (this.detail.id != null) {
              updateDeptInfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addDeptInfo(this.detail).then(response => {
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

      /** 取消按钮 */
      cancel() {
        if (this.deptInfoId) {
          this.getDetail(this.deptInfoId)
          this.$emit('update:disabled', true)
        }
        else{
          this.$emit('handleClose')
        }
      },

      /** 关闭按钮 */
      handleClose() {
        this.$emit('handleClose')
      },
      ApiCostDeptInfoTreeselect(){
        let self = this;
        costDeptInfoTreeselect({companyId:self.companyId,itemNo:self.itemNo}).then(resp=>{
          self.deptInfos = resp.data

        });
      }
    },
    watch:{
      '$store.state.item.itemNo'(newVal,oldVal){
        this.$nextTick(()=>{
          this.itemNo = this.$store.state.item.itemNo
          this.companyId = this.$store.state.item.companyId
        })
      }
    }
  }
</script>
