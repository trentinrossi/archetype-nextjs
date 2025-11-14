'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account } from '@/types/account';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button } from '@/components/ui';

export default function AccountsPage() {
  const router = useRouter();
  const [accounts, setAccounts] = useState<Account[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [page, setPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const pageSize = 20;

  const fetchAccounts = useCallback(async (currentPage: number = 0) => {
    try {
      setLoading(true);
      const data = await accountService.getAccounts(currentPage, pageSize, 'accountId,asc');
      setAccounts(data.content);
      setTotalPages(data.totalPages);
      setTotalElements(data.totalElements);
      setError(null);
    } catch (err) {
      setError('Failed to load accounts');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchAccounts(page);
  }, [page, fetchAccounts]);

  const handleDelete = async (id: number) => {
    if (!confirm('Are you sure you want to delete this account?')) return;
    
    try {
      await accountService.deleteAccount(id);
      fetchAccounts(page);
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const formatCurrency = (value: number | undefined): string => {
    if (value === undefined || value === null) return '$0.00';
    return `$${value.toFixed(2)}`;
  };

  const formatDate = (dateString: string | undefined): string => {
    if (!dateString) return '';
    return new Date(dateString).toLocaleDateString();
  };

  if (loading && accounts.length === 0) return <div className="p-6">Loading...</div>;
  if (error && accounts.length === 0) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Accounts</h1>
          <p className="text-gray-600 mt-1">Total: {totalElements} accounts</p>
        </div>
        <div className="flex gap-2">
          <Button onClick={() => router.push('/accounts/view')}>
            View Account
          </Button>
          <Button onClick={() => router.push('/accounts/update')}>
            Update Account
          </Button>
          <Button onClick={() => router.push('/accounts/new')}>
            Create Account
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg overflow-hidden">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Account ID</TableHead>
              <TableHead>Customer ID</TableHead>
              <TableHead>Status</TableHead>
              <TableHead>Current Balance</TableHead>
              <TableHead>Credit Limit</TableHead>
              <TableHead>Available Credit</TableHead>
              <TableHead>Open Date</TableHead>
              <TableHead>Actions</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {accounts.length === 0 ? (
              <TableRow>
                <TableCell colSpan={8} className="text-center text-gray-500 py-8">
                  No accounts found
                </TableCell>
              </TableRow>
            ) : (
              accounts.map((account) => (
                <TableRow key={account.accountId}>
                  <TableCell>
                    <div className="cursor-pointer font-medium" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                      {account.accountId}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                      {account.customerId}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                      <span className={`px-2 py-1 rounded text-xs font-semibold ${
                        account.activeStatus === 'Y' ? 'bg-green-100 text-green-800' : 'bg-gray-100 text-gray-800'
                      }`}>
                        {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                      </span>
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                      {formatCurrency(account.currentBalance)}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                      {formatCurrency(account.creditLimit)}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                      {formatCurrency(account.availableCredit)}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                      {formatDate(account.openDate)}
                    </div>
                  </TableCell>
                  <TableCell>
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
                  </TableCell>
                </TableRow>
              ))
            )}
          </TableBody>
        </Table>
      </div>

      {totalPages > 1 && (
        <div className="flex justify-between items-center mt-6">
          <Button
            variant="secondary"
            onClick={() => setPage(page - 1)}
            disabled={page === 0}
          >
            Previous
          </Button>
          <span className="text-gray-600">
            Page {page + 1} of {totalPages}
          </span>
          <Button
            variant="secondary"
            onClick={() => setPage(page + 1)}
            disabled={page >= totalPages - 1}
          >
            Next
          </Button>
        </div>
      )}
    </div>
  );
}
