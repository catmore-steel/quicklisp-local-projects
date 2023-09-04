<template>
  <el-form :model="attachInfoForm" :rules="attachInfoForm.eachRules" ref="attachInfoForm" label-width="0px">
    <!--表单添加-->
    <el-row :gutter="20">
      <el-col :span="3"><el-input :disabled="attachInfoForm.eachList.length == 0? false :true " class="el-input" placeholder="RelatePage" v-model="relatePage"></el-input></el-col>
      <el-col :span="2"> <el-button type="success" @click="AddListRow()" round>添加附件模块</el-button></el-col>
    </el-row>


    <el-table :data="attachInfoForm.eachList" height="560" >
      <el-table-column label="* 附件名称" align="center" >
        <template slot-scope="scope">
          <el-form-item :prop="'eachList.' + scope.$index + '.attachName'" :rules="attachInfoForm.eachRules.attachName"  >
            <el-input  v-model="scope.row.attachName" >
              <i slot="suffix" class="el-input__icon el-icon-search"></i>
            </el-input>
          </el-form-item>
        </template>
      </el-table-column>
      <el-table-column label="* 是否多选" align="center" width="320">
        <template slot-scope="scope">
          <el-form-item :prop="'eachList.' + scope.$index + '.checkMany'" :rules="attachInfoForm.eachRules.checkMany">
            <el-select v-model="scope.row.checkMany"  placeholder="请选择">
              <el-option
                v-for="dict in checkManyOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </template>
      </el-table-column>
      <el-table-column label="* 附件类型" align="center" width="320">
        <template slot-scope="scope">
          <el-form-item :prop="'eachList.' + scope.$index + '.listType'" :rules="attachInfoForm.eachRules.listType">
            <el-select v-model="scope.row.listType" placeholder="请选择">
              <el-option
                v-for="dict in listTypeOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </template>
      </el-table-column>
      <el-table-column label="* RelatePage" align="center" >
        <template slot-scope="scope">
          <el-form-item :prop="'eachList.' + scope.$index + '.relatePage'" :rules="attachInfoForm.eachRules.relatePage">
            <el-input v-model="scope.row.relatePage" :readonly="true"   ></el-input>
          </el-form-item>
        </template>
      </el-table-column>
      <el-table-column label="* RelateKey" align="center" >
        <template slot-scope="scope">
          <el-form-item :prop="'eachList.' + scope.$index + '.relateKey'" :rules="attachInfoForm.eachRules.relateKey">
          <el-input v-model="scope.row.relateKey"></el-input>
          </el-form-item>
        </template>
      </el-table-column>
      <el-table-column label="操作"  align="center" >
        <template slot-scope="scope">
          <el-form-item>
            <el-button  type="danger" @click="handleDelete(scope.$index)">删除</el-button>
          </el-form-item>
        </template>
      </el-table-column>
    </el-table>
  </el-form>
</template>

<script>
import {getGenTable,getGenAttachInfo } from "@/api/tool/gen";

export default {
  name: 'AttachInfoForm',


  data() {
    return {
      //是否多选字典数据
      checkManyOptions: [],
      //附件类型字典数据
      listTypeOptions: [],
      //附件索引
      attachIndex: 0,
      //附件页面
      relatePage:null,
      //附件信息表单域
      attachInfoForm: {
        eachList: [],
        eachRules: {
          attachName: [
            {required: true, message: '附件名称不能为空', trigger: 'blur'}
          ] ,
          checkMany: [
            { required: true, message: '是否多选不能为空', trigger: 'blur' }
          ],
          listType: [
            { required: true, message: '附件类型不能为空', trigger: 'blur' }
          ],
          relatePage: [
            { required: true, message: 'RelatePage不能为空', trigger: 'blur' }
          ],
          relateKey: [
            { required: true, message: 'RelateKey不能为空', trigger: 'blur' }
          ]
        }
      }, // 存储表格table信息
    }
  },
  created() {
    this.getDicts("sys_yes_no").then(response => {
      this.checkManyOptions = response.data;
      this.checkMany = this.checkManyOptions[0].dictLabel
    });
    this.getDicts("sys_attach_type").then(response => {
      this.listTypeOptions = response.data;
      this.listType = this.listTypeOptions[0].dictLabel
    });
    const tableId = this.$route.params && this.$route.params.tableId;
    if (tableId) {
      // 获取表详细信息
      getGenAttachInfo(tableId).then(res => {
        for (const data of res) {
           this.relatePage =  data.relatePage;
          this.attachInfoForm.eachList.push(data);
        };
      });
    }
  },
  methods: {
    //点击加号添加一行数据
    AddListRow() {
      if (this.relatePage == null || this.relatePage == ""){
        this.msgError("请先输入relatePage 选择框！ ");
        return false;
      }
      this.attachIndex += 1;
      this.attachInfoForm.eachList.push({
        "attachName": "附件" + this.attachIndex,
        "checkMany": "Y",
        "listType": "text",
        "relatePage": this.relatePage,
        "relateKey": "fj" + this.attachIndex
      });
    },
    handleDelete(index) {
      this.attachInfoForm.eachList.splice(index, 1);
    },
  }
};
</script>

<style lang="scss" scoped>
.dialogDiv {
  height: 300px;
  overflow: auto;
}
input[readonly]{
  background-color: rgb(26, 179, 148);
}
</style>
