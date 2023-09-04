<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="8">
          <el-form-item label="客户名称" prop="companyId">
            <el-select v-model="detail.companyId" placeholder="请输入客户名称">
              <el-option v-for="dict in baseCompanyInfos" :key="dict.id" :label="dict.companyName" :value="dict.id" ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="申报年份" prop="applyDate">

            <el-date-picker
              clearable size="small"
              v-model="detail.applyDate"
              class="form-control"
              type="year"
              value-format="yyyy"
              placeholder="请输入申报年份">
            </el-date-picker>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="项目编号" prop="itemNo">
            <el-input v-model="detail.itemNo" placeholder="项目编号自动生成" readonly/>
          </el-form-item>
        </el-col>

      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="项目名称" prop="itemName">
            <el-input v-model="detail.itemName" placeholder="项目名称自动生成" readonly/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="项目状态">
            <el-select v-model="detail.itemStatus" placeholder="请选择项目状态" class="form-control" clearable filterable>
              <el-option v-for="dict in projectStatusList" :key="dict.dictValue" :label="dict.dictLabel" :value="dict.dictValue" ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="项目负责人" prop="userId">
            <jq-user-select :check-many="false" :agent.sync="detail.userId"></jq-user-select>
          </el-form-item>
        </el-col>

      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="客户联系人" prop="contactName">
            <el-input v-model="detail.contactName" placeholder="请输入客户联系人" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="客户联系电话" prop="contactPhone">
            <el-input v-model="detail.contactPhone" placeholder="请输入客户联系电话" />
          </el-form-item>
        </el-col>

      </el-row>
      <el-row>


      </el-row>
      <div class="fixed_coperate" v-show="!disabled">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="handleClose">取 消</el-button>
      </div>
    </el-form>
  </div>
</template>

<script>
  import {isNullOrEmpty} from '@/utils/jq'
  import {getBaseItemInfo, delInfo, addBaseItemInfo, updateBaseItemInfo} from "@/api/project/baseItemInfo.js";
  import {projectCompanyinfoFindList} from "@/api/project/companyinfo.js";
  import {selectDeptUser} from "@/api/system/user.js";

  export default {
    name: 'infoUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules: {
          companyId: [
            { required: true, message: "所属客户不能为空", trigger: "blur" }
          ],
          applyDate: [
            { required: true, message: "申报年份不能为空", trigger: "blur" }
          ]
        },
        baseCompanyInfos:[],
        projectStatusList:[],
        users:[]
      }
    },
    props: {
      infoId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      infoId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.infoId)
      this.apiProjectCompanyinfoFindList();
      this.getDicts('project_status').then(response => { this.projectStatusList = response.data });
      this.apiSelectDeptUser();
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          itemNo: null,
          itemName: null,
          applyDate: null,
          contactName: null,
          contactPhone: null,
          userId: null,
          itemStatus: null,
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

      /** 获取项目管理;详情 */
      getDetail(infoId) {
        if (isNullOrEmpty(infoId)) {
          this.reset()
        } else {
          getBaseItemInfo(infoId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            this.detail.delFlag=0;
            if (this.detail.id != null) {
              updateBaseItemInfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addBaseItemInfo(this.detail).then(response => {
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

      /** 关闭按钮 */
      handleClose() {
        this.$emit('handleClose')
      },
      apiProjectCompanyinfoFindList(){
        let self = this;
        projectCompanyinfoFindList().then(resp=>{
          self.baseCompanyInfos = resp.data
        })


      },
      apiSelectDeptUser() {
        let self = this;
        selectDeptUser().then(resp=>{
          self.users = resp.data;
        })

      }
    }
  }
</script>
