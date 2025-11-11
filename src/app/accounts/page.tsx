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
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const pageSize = 20;

  const fetchAccounts = useCallback(async (page: number = 0) => {
    try {
      setLoading(true);
      const data = await accountService.getAccounts(page, pageSize, 'createdAt,desc');
      setAccounts(data.content);
      setCurrentPage(data.number);
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
    fetchAccounts(currentPage);
  }, [fetchAccounts, currentPage]);

  const handleDelete = async (accountId: string) => {
    if (!confirm(`Are you sure you want to delete account ${accountId}?`)) return;
    
    try {
      await accountService.deleteAccount(accountId);
      fetchAccounts(currentPage);
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const handlePreviousPage = () => {
    if (currentPage > 0) {
      setCurrentPage(currentPage - 1);
    }
  };

  const handleNextPage = () => {
    if (currentPage < totalPages - 1) {
      setCurrentPage(currentPage + 1);
    }
  };

  if (loading && accounts.length === 0) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Accounts</h1>
          <p className="text-gray-600 mt-1">
            Showing {accounts.length} of {totalElements} accounts
          </p>
        </div>
        <Button onClick={() => router.push('/accounts/new')}>
          Create Account
        </Button>
      </div>
      
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>Account ID</TableHead>
            <TableHead>Current Balance</TableHead>
            <TableHead>Status</TableHead>
            <TableHead>Created At</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {accounts.length === 0 ? (
            <TableRow>
              <TableCell colSpan={5} className="text-center text-gray-500 py-8">
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
                    ${account.currentBalance.toFixed(2)}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                    <span className={`px-2 py-1 rounded text-xs font-semibold ${
                      account.hasPositiveBalance 
                        ? 'bg-yellow-100 text-yellow-800' 
                        : 'bg-green-100 text-green-800'
                    }`}>
                      {account.hasPositiveBalance ? 'Balance Due' : 'Paid'}
                    </span>
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.accountId}`)}>
                    {new Date(account.createdAt).toLocaleDateString()}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="flex gap-2">
                    <Button 
                      size="sm" 
                      onClick={(e) => {
                        e.stopPropagation();
                        router.push(`/accounts/${account.accountId}`);
                      }}
                    >
                      View
                    </Button>
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

      {totalPages > 1 && (
        <div className="flex justify-between items-center mt-6">
          <Button 
            variant="secondary" 
            onClick={handlePreviousPage} 
            disabled={currentPage === 0}
          >
            Previous
          </Button>
          <span className="text-gray-600">
            Page {currentPage + 1} of {totalPages}
          </span>
          <Button 
            variant="secondary" 
            onClick={handleNextPage} 
            disabled={currentPage >= totalPages - 1}
          >
            Next
          </Button>
        </div>
      )}
    </div>
  );
}
