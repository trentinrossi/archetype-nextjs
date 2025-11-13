'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { userService } from '@/services/userService';
import { User } from '@/types/user';
import { Button } from '@/components/ui';

export default function UserDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  const [currentDate, setCurrentDate] = useState('');
  const [currentTime, setCurrentTime] = useState('');

  useEffect(() => {
    const currentUser = userService.getCurrentUser();
    if (!currentUser || currentUser.userType !== 'A') {
      router.push('/login');
      return;
    }

    if (params.id) {
      fetchUser(params.id as string);
    }

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, [params.id, router]);

  const updateDateTime = () => {
    const now = new Date();
    setCurrentDate(now.toLocaleDateString('en-US', { 
      month: '2-digit', 
      day: '2-digit', 
      year: 'numeric' 
    }));
    setCurrentTime(now.toLocaleTimeString('en-US', { 
      hour: '2-digit', 
      minute: '2-digit', 
      second: '2-digit',
      hour12: false 
    }));
  };

  const fetchUser = async (id: string) => {
    try {
      const data = await userService.getUserById(parseInt(id));
      setUser(data);
    } catch (err) {
      console.error('Failed to load user:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!user) return;
    if (!confirm(`Are you sure you want to delete user ${user.userId}?`)) return;
    
    try {
      await userService.deleteUser(user.id);
      router.push('/users');
    } catch (err) {
      alert('Failed to delete user');
      console.error(err);
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg">Loading...</div>
      </div>
    );
  }

  if (!user) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg">User not found</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <div className="bg-blue-900 text-white p-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">AWS Mainframe Modernization</h1>
            <h2 className="text-lg">CardDemo - User Details</h2>
          </div>
          <div className="text-right">
            <div className="text-sm">Date: {currentDate}</div>
            <div className="text-sm">Time: {currentTime}</div>
          </div>
        </div>
      </div>

      <div className="flex-1 p-6">
        <div className="max-w-4xl mx-auto">
          <div className="bg-white shadow-lg rounded-lg p-8">
            <div className="flex justify-between items-center mb-6">
              <h3 className="text-xl font-bold text-gray-800">User Information</h3>
              <div className="flex gap-2">
                <Button onClick={() => router.push(`/users/${user.id}/edit`)}>
                  Edit
                </Button>
                <Button variant="danger" onClick={handleDelete}>
                  Delete
                </Button>
                <Button variant="secondary" onClick={() => router.push('/users')}>
                  Back to List
                </Button>
              </div>
            </div>
            
            <div className="space-y-6">
              <div className="grid grid-cols-2 gap-6">
                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">User ID</label>
                  <p className="text-gray-900 text-lg">{user.userId}</p>
                </div>
                
                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">User Type</label>
                  <p className="text-gray-900">
                    <span className={`px-3 py-1 rounded text-sm font-semibold ${
                      user.userType === 'A' ? 'bg-purple-100 text-purple-800' : 'bg-blue-100 text-blue-800'
                    }`}>
                      {user.userTypeDisplay}
                    </span>
                  </p>
                </div>
              </div>

              <div className="grid grid-cols-2 gap-6">
                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">First Name</label>
                  <p className="text-gray-900">{user.firstName}</p>
                </div>
                
                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">Last Name</label>
                  <p className="text-gray-900">{user.lastName}</p>
                </div>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Full Name</label>
                <p className="text-gray-900">{user.fullName}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Authentication Status</label>
                <p className="text-gray-900">
                  <span className={`px-3 py-1 rounded text-sm font-semibold ${
                    user.authenticated ? 'bg-green-100 text-green-800' : 'bg-gray-100 text-gray-800'
                  }`}>
                    {user.authenticatedDisplay}
                  </span>
                </p>
              </div>

              <div className="grid grid-cols-2 gap-6">
                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">Created At</label>
                  <p className="text-gray-900">{new Date(user.createdAt).toLocaleString()}</p>
                </div>
                
                <div>
                  <label className="block text-sm font-semibold text-gray-700 mb-1">Updated At</label>
                  <p className="text-gray-900">{new Date(user.updatedAt).toLocaleString()}</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
