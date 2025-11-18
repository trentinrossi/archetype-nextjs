'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { creditCardService } from '@/services/creditCardService';
import { Account } from '@/types/account';
import { CreditCard } from '@/types/creditCard';
import { Button, Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [creditCards, setCreditCards] = useState<CreditCard[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.id) {
      fetchAccountDetails(params.id as string);
    }
  }, [params.id]);

  const fetchAccountDetails = async (id: string) => {
    try {
      setLoading(true);
      const accountData = await accountService.getAccountById(id);
      setAccount(accountData);

      const cardsData = await creditCardService.getCreditCardsByAccountId(id);
      setCreditCards(cardsData);
      setError(null);
    } catch (err: any) {
      setError(err.message || 'Failed to load account details');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!account) return;
    if (!confirm('Are you sure you want to delete this account? This will also delete all associated credit cards.')) return;

    try {
      await accountService.deleteAccount(account.accountId.toString());
      router.push('/accounts');
    } catch (err: any) {
      alert(err.message || 'Failed to delete account');
      console.error(err);
    }
  };

  const handleViewCard = (cardNumber: string) => {
    router.push(`/credit-cards/${cardNumber}`);
  };

  if (loading) {
    return <div className="p-6">Loading account details...</div>;
  }

  if (error) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
        <Button variant="secondary" onClick={() => router.push('/accounts')}>
          Back to Accounts
        </Button>
      </div>
    );
  }

  if (!account) {
    return (
      <div className="p-6">
        <div className="text-gray-600 mb-4">Account not found</div>
        <Button variant="secondary" onClick={() => router.push('/accounts')}>
          Back to Accounts
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Details</h1>
        <div className="flex gap-2">
          <Button variant="danger" onClick={handleDelete}>
            Delete Account
          </Button>
          <Button variant="secondary" onClick={() => router.push('/accounts')}>
            Back to Accounts
          </Button>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-lg font-semibold mb-4">Account Information</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Account ID
            </label>
            <p className="text-gray-900">{account.accountId}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Formatted Account ID
            </label>
            <p className="text-gray-900">{account.formattedAccountId}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Credit Card Count
            </label>
            <p className="text-gray-900">{account.creditCardCount}</p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Created At
            </label>
            <p className="text-gray-900">
              {new Date(account.createdAt).toLocaleString()}
            </p>
          </div>

          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Updated At
            </label>
            <p className="text-gray-900">
              {new Date(account.updatedAt).toLocaleString()}
            </p>
          </div>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-lg font-semibold">Associated Credit Cards</h2>
          <Button
            size="sm"
            onClick={() => router.push(`/credit-cards/new?accountId=${account.accountId}`)}
          >
            Add Credit Card
          </Button>
        </div>

        {creditCards.length === 0 ? (
          <p className="text-gray-500 text-center py-8">
            No credit cards associated with this account
          </p>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Card Number</TableHead>
                <TableHead>Embossed Name</TableHead>
                <TableHead>Expiration</TableHead>
                <TableHead>Status</TableHead>
                <TableHead>Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {creditCards.map((card) => (
                <TableRow key={card.cardNumber}>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewCard(card.cardNumber)}
                    >
                      {card.maskedCardNumber}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewCard(card.cardNumber)}
                    >
                      {card.embossedName}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewCard(card.cardNumber)}
                    >
                      {card.expirationDateFormatted}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewCard(card.cardNumber)}
                    >
                      <span
                        className={`px-2 py-1 rounded text-xs font-semibold ${
                          card.isActive
                            ? 'bg-green-100 text-green-800'
                            : 'bg-gray-100 text-gray-800'
                        }`}
                      >
                        {card.isActive ? 'Active' : 'Inactive'}
                      </span>
                    </div>
                  </TableCell>
                  <TableCell>
                    <Button
                      size="sm"
                      onClick={(e) => {
                        e.stopPropagation();
                        handleViewCard(card.cardNumber);
                      }}
                    >
                      View Details
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        )}
      </div>
    </div>
  );
}
