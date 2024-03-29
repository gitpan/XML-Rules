use strict;

use XML::Rules;

my $parser = XML::Rules->new(
	stripspaces => 7,
	rules => [
		'_default' => 'content',
		'Dev_Info' => sub {
			print "$_[1]->{dev_name}\t$_[1]->{configuration}\n";
			return;
		},
		'Device' => '',
	],
	start_rules => [
		'Hyper,RAID-5_Device,Back_End,Mirror_Set,Front_End,Product,Label,Flags,Capacity' => 'skip',
	],
);

$parser->parse(\*DATA);

__DATA__
<?xml version="1.0" standalone="yes" ?>
<SymCLI_ML>
  <Symmetrix>
    <Symm_Info>
      <symid>000290101935</symid>
    </Symm_Info>
    <Device>
      <Dev_Info>
        <pd_name>Not Visible</pd_name>
        <dev_name>0040</dev_name>
        <configuration>RAID-5</configuration>
        <attached_bcv>N/A</attached_bcv>
        <emulation>CKD-3390</emulation>
        <status>Ready</status>
        <sa_status>N/A</sa_status>
        <service_state>Normal</service_state>
        <ssid>0xD800</ssid>
        <cuimage>0x00</cuimage>
      </Dev_Info>
      <Attached>
        <BCV>N/A</BCV>
        <VDEV>N/A</VDEV>
      </Attached>
      <Product>
        <vendor>
        </vendor>
        <name>
        </name>
        <revision>
        </revision>
        <serial_id>N/A</serial_id>
        <symid>000290101935</symid>
      </Product>
      <Label>
        <type>N/A</type>
        <defined_label>N/A</defined_label>
      </Label>
      <Flags>
        <ckd>True</ckd>
        <worm_enabled>False</worm_enabled>
        <worm_protected>False</worm_protected>
        <dynamic_spare_invoked>False</dynamic_spare_invoked>
        <dynamic_rdf_capability>None</dynamic_rdf_capability>
        <star_mode>False</star_mode>
        <star_recovery_capability>None</star_recovery_capability>
        <star_recovery_state>N/A</star_recovery_state>
        <radiant_managed>False</radiant_managed>
        <restricted_access_dev>False</restricted_access_dev>
        <rdb_checksum_enabled>False</rdb_checksum_enabled>
        <non_exclusive_access>False</non_exclusive_access>
        <scsi3_persist_res>Disabled</scsi3_persist_res>
        <vcm>False</vcm>
        <symmetrix_filesystem>False</symmetrix_filesystem>
        <snap_save_device>False</snap_save_device>
        <gatekeeper>False</gatekeeper>
        <meta>None</meta>
      </Flags>
      <Capacity>
        <block_size>56664</block_size>
        <cylinders>1113</cylinders>
        <tracks>16695</tracks>
        <blocks>16695</blocks>
        <megabytes>902</megabytes>
        <kilobytes>923833</kilobytes>
      </Capacity>
      <Front_End>
        <Port>
          <pd_name>Not Visible</pd_name>
          <director>03A</director>
          <director_type>FICON</director_type>
          <powerpath_type>N/A</powerpath_type>
          <port>0</port>
          <port_status>N/A</port_status>
          <tid>0</tid>
          <lun>0</lun>
          <host_lun>N/A</host_lun>
          <base_address>0</base_address>
          <alias_count>0</alias_count>
        </Port>
        <Port>
          <pd_name>Not Visible</pd_name>
          <director>04A</director>
          <director_type>FICON</director_type>
          <powerpath_type>N/A</powerpath_type>
          <port>0</port>
          <port_status>N/A</port_status>
          <tid>0</tid>
          <lun>0</lun>
          <host_lun>N/A</host_lun>
          <base_address>0</base_address>
          <alias_count>0</alias_count>
        </Port>
        <Port>
          <pd_name>Not Visible</pd_name>
          <director>13A</director>
          <director_type>FICON</director_type>
          <powerpath_type>N/A</powerpath_type>
          <port>0</port>
          <port_status>N/A</port_status>
          <tid>0</tid>
          <lun>0</lun>
          <host_lun>N/A</host_lun>
          <base_address>0</base_address>
          <alias_count>0</alias_count>
        </Port>
        <Port>
          <pd_name>Not Visible</pd_name>
          <director>14A</director>
          <director_type>FICON</director_type>
          <powerpath_type>N/A</powerpath_type>
          <port>0</port>
          <port_status>N/A</port_status>
          <tid>0</tid>
          <lun>0</lun>
          <host_lun>N/A</host_lun>
          <base_address>0</base_address>
          <alias_count>0</alias_count>
        </Port>
      </Front_End>
      <Mirror_Set>
        <Mirror>
          <number>1</number>
          <type>RAID-5</type>
          <status>Ready</status>
          <invalid_tracks>0</invalid_tracks>
        </Mirror>
        <Mirror>
          <number>2</number>
          <type>RAID-5</type>
          <status>Ready</status>
          <invalid_tracks>0</invalid_tracks>
        </Mirror>
        <Mirror>
          <number>3</number>
          <type>N/A</type>
          <status>N/A</status>
          <invalid_tracks>0</invalid_tracks>
        </Mirror>
        <Mirror>
          <number>4</number>
          <type>N/A</type>
          <status>N/A</status>
          <invalid_tracks>0</invalid_tracks>
        </Mirror>
      </Mirror_Set>
      <Back_End>
        <Hyper>
          <type>RAID-5</type>
          <status>Ready</status>
          <number>N/A</number>
          <Disk>
            <director>N/A</director>
            <interface>N/A</interface>
            <tid>N/A</tid>
            <volume_number>N/A</volume_number>
          </Disk>
        </Hyper>
        <Hyper>
          <type>RAID-5</type>
          <status>Ready</status>
          <number>N/A</number>
          <Disk>
            <director>N/A</director>
            <interface>N/A</interface>
            <tid>N/A</tid>
            <volume_number>N/A</volume_number>
          </Disk>
        </Hyper>
      </Back_End>
      <RAID-5_Device>
        <RAID5_Dev_Info>
          <tracks_per_stripe>4</tracks_per_stripe>
          <ready_state>ReadyNoOtherMirror</ready_state>
          <writeprotect_state>EnabledNoOtherMirror</writeprotect_state>
          <member_num_of_failing_dev>None</member_num_of_failing_dev>
          <member_which_invoked_spare>None</member_which_invoked_spare>
          <disk_director_num_which_owns_spare>-1</disk_director_num_which_owns_spare>
          <disk_director_ident_which_owns_spare>N/A</disk_director_ident_which_owns_spare        >
          <copy_direction>N/A</copy_direction>
        </RAID5_Dev_Info>
        <Hyper>
          <director>01A</director>
          <interface>D</interface>
          <tid>5</tid>
          <da_vol_num>444</da_vol_num>
          <hyper_num>56</hyper_num>
          <hyper_capacity_in_mb>307</hyper_capacity_in_mb>
          <member_num>4</member_num>
          <member_status>RW</member_status>
          <spare_status>N/A</spare_status>
          <disk_group_num>2</disk_group_num>
          <disk_capacity_in_mb>140014</disk_capacity_in_mb>
        </Hyper>
        <Hyper>
          <director>15A</director>
          <interface>D</interface>
          <tid>5</tid>
          <da_vol_num>468</da_vol_num>
          <hyper_num>56</hyper_num>
          <hyper_capacity_in_mb>307</hyper_capacity_in_mb>
          <member_num>1</member_num>
          <member_status>RW</member_status>
          <spare_status>N/A</spare_status>
          <disk_group_num>2</disk_group_num>
          <disk_capacity_in_mb>140014</disk_capacity_in_mb>
        </Hyper>
        <Hyper>
          <director>02C</director>
          <interface>C</interface>
          <tid>5</tid>
          <da_vol_num>66</da_vol_num>
          <hyper_num>56</hyper_num>
          <hyper_capacity_in_mb>307</hyper_capacity_in_mb>
          <member_num>3</member_num>
          <member_status>RW</member_status>
          <spare_status>N/A</spare_status>
          <disk_group_num>2</disk_group_num>
          <disk_capacity_in_mb>140014</disk_capacity_in_mb>
        </Hyper>
        <Hyper>
          <director>16C</director>
          <interface>C</interface>
          <tid>5</tid>
          <da_vol_num>66</da_vol_num>
          <hyper_num>56</hyper_num>
          <hyper_capacity_in_mb>307</hyper_capacity_in_mb>
          <member_num>2</member_num>
          <member_status>RW</member_status>
          <spare_status>N/A</spare_status>
          <disk_group_num>2</disk_group_num>
          <disk_capacity_in_mb>140014</disk_capacity_in_mb>
        </Hyper>
      </RAID-5_Device>
    </Device>
  </Symmetrix>
</SymCLI_ML>
