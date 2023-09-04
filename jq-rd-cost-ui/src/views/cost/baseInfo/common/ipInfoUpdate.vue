<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="8">
          <el-form-item label="资产编号" prop="ipNo">
            <el-input v-model="detail.ipNo" placeholder="自动生成" disabled/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="资产名称" prop="ipName">
            <el-input v-model="detail.ipName" placeholder="请输入资产名称"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="资产类型" prop="ipType">
            <el-select v-model="detail.ipType" placeholder="请选择资产类型" class="form-control" clearable filterable>
              <el-option
                v-for="dict in ipTypeOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="所属部门" class="demonstration" prop="deptId">
            <treeselect v-model="detail.deptId" :multiple="false" :options="deptIdOptions" :show-count="true" class="form-control"
                        placeholder="请选择使用部门" :disabled="disabled"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="登记编号" prop="registeNo">
            <el-input v-model="detail.registeNo" placeholder="请输入登记编号"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="取得时间" prop="getDate">
            <el-date-picker
              clearable size="small"
              v-model="detail.getDate"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择取得时间">
            </el-date-picker>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="客户名称" prop="companyId">
            <jq-select-company :companyId.sync="detail.companyId"></jq-select-company>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="无形资产价值" prop="ipMoney">
            <el-input v-model="detail.ipMoney" placeholder="请输入无形资产价值"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="摊销月数" prop="usableMonths">
            <el-input v-model="detail.usableMonths" placeholder="请输入摊销月数"/>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="月摊销额" prop="monthlyAmount">
            <el-input v-model="calculatedMonthlyAmount" placeholder="请输入月摊销额" disabled/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="起始日期" prop="startDate">
            <el-date-picker
              clearable size="small"
              v-model="detail.startDate"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择起始日期">
            </el-date-picker>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="结束时间" prop="endDate">
            <el-date-picker
              clearable size="small"
              v-model="detail.endDate"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择结束时间">
            </el-date-picker>
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="24">
          <el-form-item label="备注" prop="remark">
            <el-input type="textarea" rows="10" v-model="detail.remark" placeholder="请输入备注"/>
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
  import { isNullOrEmpty } from '@/utils/jq'
  import { getIpInfo, addIpInfo, updateIpInfo } from '@/api/cost/ipInfo'
  import { getDeptNameInfo } from '@/api/cost/deptInfo'

  export default {
    name: 'ipInfoUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        paramForm: {
          companyId: null
        },
        // 部门下拉框
        deptIdOptions: [],
        // 设备类型字典
        ipTypeOptions: [],
        detail: {},
        //表单校验
        rules: {
          ipNo: [
            { required: false, message: '资产编号不能为空', trigger: 'blur' }
          ],
          ipName: [
            { required: true, message: '资产名称不能为空', trigger: 'blur' }
          ],
          ipType: [
            { required: true, message: '资产类型不能为空', trigger: 'change' }
          ],
          deptId: [
            { required: true, message: '部门不能为空', trigger: 'blur' }
          ],
          ipMoney: [
            { required: true, message: '无形资产价值不能为空', trigger: 'blur' }
          ],
          startDate: [
            { required: true, message: '起始日期不能为空', trigger: 'blur' }
          ],
          usableMonths: [
            { required: true, message: '摊销月数不能为空', trigger: 'blur' }
          ],
          monthlyAmount: [
            { required: true, message: '月摊销额不能为空', trigger: 'blur' }
          ],
        }
      }
    },
    props: {
      ipInfoId: {
        type: Number
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      ipInfoId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    computed: {
      calculatedMonthlyAmount() {
        const ipMoney = parseFloat(this.detail.ipMoney);
        const usableMonths = parseInt(this.detail.usableMonths);
        if (isNaN(ipMoney) || isNaN(usableMonths) || usableMonths === 0) {
          this.detail.monthlyAmount = ''; // 无效输入时将 monthlyAmount 置为空字符串
        } else {
          this.detail.monthlyAmount = (ipMoney / usableMonths).toFixed(2); // 四舍五入保留两位小数
        }
        return this.detail.monthlyAmount; // 返回月均摊销额的计算结果
      }
    },
    created() {
      //资产类型字典
      this.getDicts('ip_type').then(response => {
        this.ipTypeOptions = response.data
      })
      let companyId = this.$store.state.item.companyId
      let itemNo = this.$store.state.item.itemNo
      this.getDeptNameInfo1(companyId,itemNo)
      this.getDetail(this.ipInfoId)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          ipNo: null,
          ipName: null,
          ipType: null,
          deptId: null,
          registeNo: null,
          ipMoney: null,
          startDate: null,
          usableMonths: null,
          monthlyAmount: null,
          getDate: null,
          endDate: null,
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
        this.resetForm('detail')
      },

      /** 获取无形资产信息详情 */
      getDetail(ipInfoId) {
        if (isNullOrEmpty(ipInfoId)) {
          this.reset()
        } else {
          getIpInfo(ipInfoId).then(response => {
            this.detail = response.data
          })
        }
      },
      /** 查询部门下拉树结构 */
      getDeptNameInfo1(companyId,itemNo) {
        getDeptNameInfo(companyId,itemNo).then(response => {
          this.deptIdOptions = response.data
        })
      },
      /** 提交按钮 */
      submitForm() {
        this.$refs['detail'].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateIpInfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              })
            } else {
              this.detail.companyId = this.$store.state.item.companyId
              this.detail.itemNo = this.$store.state.item.itemNo
              addIpInfo(this.detail).then(response => {
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              })
            }
          }
        })
      },
      /** 关闭按钮 */
      handleClose() {
        this.$emit('handleClose')
      }

    }
  }
</script>
