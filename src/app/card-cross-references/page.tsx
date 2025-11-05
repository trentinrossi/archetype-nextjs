'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { CardCrossReference } from '@/types/cardCrossReference';
import { cardCrossReferenceService } from '@/services/cardCrossReferenceService';

export default function CardCrossReferencesPage() {
  const router = useRouter();
  const [searchType, setSearchType] = useState<'card' | 'account'>('card');
  const [searchValue, setSearchValue] = useState('');
  const [results, setResults] = useState<CardCrossReference[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSearch = async () => {
    if (!searchValue) {
      setError('Please enter a search value');
      return;
    }

    try {
      setLoading(true);
      setError(null);
      
      if (searchType === 'card') {
        const result = await cardCrossReferenceService.getCardCrossReferenceByCardNumber(searchValue);
        setResults([result]);
      } else {
        const resultList = await cardCrossReferenceService.getCardCrossReferencesByAccountId(parseInt(searchValue));
        setResults(resultList);
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to search');
      setResults([]);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (cardNumber: string) => {
    if (!confirm('Are you sure you want to delete this cross reference?')) return;
    try {
      await cardCrossReferenceService.deleteCardCrossReference(cardNumber);
      handleSearch();
    } catch (err) {
      alert('Failed to delete cross reference');
    }
  };

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Card Cross References</h1>
          <p className="text-sm text-gray-600 mt-1">Manage card-account relationships</p>
        </div>
        <Button onClick={() => router.push('/card-cross-references/new')}>Create Cross Reference</Button>
      </div>

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h3 className="text-lg font-semibold mb-4">Search Cross References</h3>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">Search By</label>
            <select
              value={searchType}
              onChange={(e) => setSearchType(e.target.value as 'card' | 'account')}
              className="w-full px-3 py-2 border border-gray-300 rounded-md"
            >
              <option value="card">Card Number</option>
              <option value="account">Account ID</option>
            </select>
          </div>
          <Input
            label={searchType === 'card' ? 'Card Number' : 'Account ID'}
            placeholder={searchType === 'card' ? 'Enter card number' : 'Enter account ID'}
            value={searchValue}
            onChange={(e) => setSearchValue(e.target.value)}
          />
          <div className="flex items-end">
            <Button onClick={handleSearch} disabled={loading} className="w-full">
              {loading ? 'Searching...' : 'Search'}
            </Button>
          </div>
        </div>
      </div>

      {error && (
        <div className="mb-6 bg-red-50 border border-red-200 rounded-lg p-4">
          <p className="text-sm text-red-700">{error}</p>
        </div>
      )}

      {results.length > 0 && (
        <div className="bg-white shadow rounded-lg overflow-hidden">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Card Number</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Account ID</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Customer ID</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Created At</th>
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Actions</th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {results.map((ref) => (
                <tr key={ref.cardNumber}>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">****{ref.cardNumber.slice(-4)}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">{ref.accountId}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">{ref.customerId}</td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                    {new Date(ref.createdAt).toLocaleDateString()}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm">
                    <Button size="sm" variant="danger" onClick={() => handleDelete(ref.cardNumber)}>
                      Delete
                    </Button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}
