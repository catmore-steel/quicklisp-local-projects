<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="120px" :disabled="disabled">
      <el-row>
        <el-col :span="8">
          <el-form-item label="企业名称" prop="companyName">
            <el-input v-model="detail.companyName" placeholder="请输入企业名称" />
          </el-form-item>

        </el-col>
        <el-col :span="2">
          <el-button class="upload" style="margin-left: 10px" @click="getBusinessInfo" type="primary"
                     title="点击获取天眼查工商信息">获取工商信息
          </el-button>
        </el-col>

      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="统一信用代码" prop="creditCode">
            <el-input v-model="detail.creditCode" placeholder="请输入统一信用代码" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="曾用名" prop="oldName">
            <el-input v-model="detail.oldName" placeholder="请输入曾用名" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="注册资本" prop="registeFee">
            <el-input v-model="detail.registeFee" placeholder="请输入注册资本" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="法人代表" prop="legalName">
            <el-input v-model="detail.legalName" placeholder="请输入法人代表" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="实缴资本" prop="subFee">
            <el-input v-model="detail.subFee" placeholder="请输入实缴资本" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="行业类别" prop="industry">
            <treeselect v-model="detail.industry" :options="industries"  class="form-control" :show-count="true" :disabled="disabled" placeholder="请选择行业类别"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="企业类型" prop="companyType">
            <el-select v-model="detail.companyType" placeholder="请选择企业类型">
              <el-option v-for="dict in regTypes" :key="dict.dictValue" :label="dict.dictLabel" :value="dict.dictValue" ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="成立日期" prop="foundDate">
            <el-date-picker
              clearable size="small"
              v-model="detail.foundDate"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择成立日期">
            </el-date-picker>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="工商注册号" prop="businessNo">
            <el-input v-model="detail.businessNo" placeholder="请输入工商注册号" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="登记机关" prop="registeDept">
            <el-input v-model="detail.registeDept" placeholder="请输入登记机关" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="行政区划" prop="divisionId">
            <el-cascader
                v-model="detail.divisionId"
                :props="{ checkStrictly: true }"
                clearable
                style="width: 100%"
                :options="regionData"
                class="el-input"
              >
              </el-cascader>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="核准日期" prop="checkDate">
            <el-date-picker
              clearable size="small"
              v-model="detail.checkDate"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择核准日期">
            </el-date-picker>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="营业期限" prop="businessTerm">
            <el-date-picker
            v-model="detail.businessTerm"
            type="daterange"
            range-separator="至"
            start-placeholder="开始日期"
            end-placeholder="结束日期"
            value-format="yyyy-MM-dd"
            placeholder="选择营业期限">
          </el-date-picker>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="经营状态">
            <el-select v-model="detail.sealStatus" placeholder="请选择企业类型">
              <el-option v-for="dict in sealStatusList" :key="dict.dictValue" :label="dict.dictLabel" :value="dict.dictValue" ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="开户银行" prop="compayBank">
            <el-input v-model="detail.compayBank" placeholder="请输入开户银行" />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="银行账号" prop="compayAccount">
            <el-input v-model="detail.compayAccount" placeholder="请输入银行账号" />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="公司电话" prop="compayPhone">
            <el-input v-model="detail.compayPhone" placeholder="请输入公司电话" />
          </el-form-item>
        </el-col>
        <el-col :span="16">
          <el-form-item label="注册地址" prop="registeAddress">
            <el-input v-model="detail.registeAddress" placeholder="请输入注册地址" />
          </el-form-item>
        </el-col>

      </el-row>
      <el-row>
        <el-col :span="24">
          <el-form-item label="经营范围" prop="sealRange">
            <el-input type="textarea" v-model="detail.sealRange" placeholder="请输入经营范围" />
          </el-form-item>
        </el-col>
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
  import {getCompanyinfo, delCompanyinfo, addCompanyinfo, updateCompanyinfo,getTianyanCompanyinfo,listPostCode} from "@/api/project/companyinfo";
  import {listIndustry, treeselectIndustry} from "@/api/conf/industry";
  import { CodeToText, regionData } from 'element-china-area-data'
  import * as entityUtils from "@/utils/entityUtils.js";
  export default {
    name: 'companyinfoUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules: {
          creditCode:[
            { required: true, message: "请输入统一信用代码", trigger: "blur" }
          ],
          companyName:[
            { required: true, message: "请输入企业名称", trigger: "blur" }
          ],
          registeFee:[
            { required: true, message: "请输入注册资本", trigger: "blur" }
          ]
        },
        industries:[],
        regTypes:[],
        regionData:regionData,
        sealStatusList:[]

      }
    },
    props: {
      companyinfoId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      companyinfoId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.companyinfoId)
      //this.apiListIndustry()
      this.apiTreeselectIndustry();
      this.getDicts('reg_type').then(response => { this.regTypes = response.data });
      this.getDicts('seal_status').then(response => { this.sealStatusList = response.data });

    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          creditCode: null,
          companyName: null,
          oldName: null,
          legalName: null,
          registeFee: null,
          subFee: null,
          industry: null,
          businessNo: null,
          registeDept: null,
          foundDate: null,
          companyType: null,
          businessTerm: null,
          divisionId: null,
          checkDate: null,
          registeAddress: null,
          sealRange: null,
          sealStatus: null,
          compayPhone: null,
          compayBank: null,
          compayAccount: null,
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

      /** 获取企业信息详情 */
      getDetail(companyinfoId) {
        if (isNullOrEmpty(companyinfoId)) {
          this.reset()
        } else {
          getCompanyinfo(companyinfoId).then(response => {
            this.detail = response.data
            entityUtils.deCascader(this.detail, ["divisionId","businessTerm"]);
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs["detail"].validate(valid => {
          if (valid) {
            entityUtils.cascader(this.detail, ["divisionId"]);
            this.detail.delFlag=0;
            this.detail.businessTerm=this.detail.businessTerm==null?null:this.detail.businessTerm.join(",");
            if (this.detail.id != null) {
              updateCompanyinfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            } else {
              addCompanyinfo(this.detail).then(response => {
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
      apiListIndustry(){
        let self = this;
        listIndustry().then(resp=>{
          self.industries = self.handleTree(resp.data, "codeNumber", "parentCodeNumber");
        })

      },
      apiTreeselectIndustry() {
        let self = this;
        treeselectIndustry().then(resp=>{
          self.industries = self.handleTree(resp.data, "codeNumber", "parentCodeNumber");
        })
      },
          /** 自动获取工商信息的按钮 */
      async getBusinessInfo() {
        if (!this.detail.companyName) {
          return this.$message.info('请先输入客户名称！')
        }
        getTianyanCompanyinfo(this.detail.companyName).then(response => {
          if (response.code == 200) {
            this.detail = { ...this.detail, ...response.data }
            this.detail.registeAddress = this.detail.address;
            this.detail.registeFee= this.detail.regMoney;
            this.detail.divisionId = this.detail.regionList;
            this.detail.sealRange = this.detail.businessScope;
            this.detail.foundDate = this.detail.regDate;
            //this.detail.foundDate = this.detail.regDate;
            
            this.getPostCode(response.data.regionList.map(item => CodeToText[item]), response.data.postCodeAddress.split(','))
          } else {
            this.msgError(response.msg)
          }
        }).catch(e => {
        })
      },
          /** 从 中国邮政https://www.ems.com.cn/postcode 获取邮政编码 */
    getPostCode(divisionNameArray, postCodeAddressArray) {
      for (const address of postCodeAddressArray) {
        let addressInfo = {
          'page': { pageNo: 1 },
          'value': {
            m: '1',
            provinces: [divisionNameArray[0], divisionNameArray[1], divisionNameArray[2]],
            addr: address
          }
        }
        listPostCode(addressInfo).then(response => {
          if (response.success) {
            this.detail.zipCode = response.value[0].POSTCODE
          }
        })
      }
      },
    }
  }
</script>
