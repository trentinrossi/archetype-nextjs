'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account } from '@/types/account';
import { Table, Button, Select } from '@/components/ui';

export default function AccountsPage() {
  const router = useRouter();
  const [accounts, setAccounts] = useState<Account[]>([]);
  const [filteredAccounts, setFilteredAccounts] = useState<Account[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [groupFilter, setGroupFilter] = useState<string>('all');

  useEffect(() => {
    fetchAccounts();
  }, []);

  useEffect(() => {
    applyFilters();
  }, [accounts, statusFilter, groupFilter]);

  const fetchAccounts = async () => {
    try {
      setLoading(true);
      const data = await accountService.getAccounts();
      setAccounts(data);
      setError(null);
    } catch (err) {
      setError('Failed to load accounts');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const applyFilters = () => {
    let filtered = [...accounts];

    if (statusFilter !== 'all') {
      filtered = filtered.filter(account => account.activeStatus === statusFilter);
    }

    if (groupFilter !== 'all') {
      filtered = filtered.filter(account => account.groupId === groupFilter);
    }

    setFilteredAccounts(filtered);
  };

  const handleDelete = async (accountId: string) => {
    if (!confirm('Are you sure you want to delete this account?')) return;

    try {
      await accountService.deleteAccount(accountId);
      fetchAccounts();
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const getUniqueStatuses = () => {
    const statuses = Array.from(new Set(accounts.map(a => a.activeStatus)));
    return statuses.map(status => ({ value: status, label: status }));
  };

  const getUniqueGroups = () => {
    const groups = Array.from(new Set(accounts.map(a => a.groupId)));
    return groups.map(group => ({ value: group, label: group }));
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Accounts</h1>
        <Button onClick={() => router.push('/accounts/new')}>
          Create Account
        </Button>
      </div>

      <div className="flex gap-4 mb-6">
        <div className="w-64">
          <Select
            label="Filter by Status"
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Statuses' },
              ...getUniqueStatuses(),
            ]}
          />
        </div>
        <div className="w-64">
          <Select
            label="Filter by Group"
            value={groupFilter}
            onChange={(e) => setGroupFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Groups' },
              ...getUniqueGroups(),
            ]}
          />
        </div>
      </div>

      {filteredAccounts.length === 0 ? (
        <div className="text-center py-8 text-gray-500">
          No accounts found matching the selected filters.
        </div>
      ) : (
        <Table
          columns={[
            { key: 'accountId', label: 'Account ID' },
            { key: 'activeStatus', label: 'Status' },
            { key: 'currentBalance', label: 'Current Balance' },
            { key: 'creditLimit', label: 'Credit Limit' },
            { key: 'groupId', label: 'Group ID' },
          ]}
          data={filteredAccounts}
          onRowClick={(account) => router.push(`/accounts/${account.accountId}`)}
          actions={(account) => (
            <div className="flex gap-2">
              <Button
                size="sm"
                onClick={(e) => {
                  e.stopPropagation();
                  router.push(`/accounts/${account.accountId}/edit`);
                }}
              >
                Edit
              </Button>
              <Button
                size="sm"
                variant="danger"
                onClick={(e) => {
                  e.stopPropagation();
                  handleDelete(account.accountId);
                }}
              >
                Delete
              </Button>
            </div>
          )}
        />
      )}
    </div>
  );
}
